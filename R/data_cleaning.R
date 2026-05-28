# ----------------------------------------------------------------------------
# R/data_cleaning.R
#
# Reusable cleaning functions for Philadelphia Real Estate Transfer Tax (RTT)
# and Office of Property Assessment (OPA) data.
#
# These functions implement the arm's-length filter, the per-year IQR trim,
# parcel-year deduplication, and the OPA snapshot merge described in
# notes/02_what_counts_as_a_sale.md.
#
# All functions are pure: they take a tibble in and return a tibble out.
# Side effects (writing files, printing summaries) live in the analysis scripts.
#
# Conventions:
#   - The assessment ratio is defined as ratio = av / sale_price (IAAO standard).
#   - Filters are applied as flags first; rows are kept until the analysis
#     script explicitly drops them. This preserves the ability to study
#     filtered-out transactions in their own right.
# ----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})


#' Parse raw RTT columns from character to typed values.
#'
#' The RTT CSV files are loaded with all columns as character to avoid
#' vroom guessing wrong on sparse or messy columns. This function applies
#' the type conversions and derives the standard date fields.
#'
#' @param rtt A tibble of raw RTT records with character columns including
#'   opa_account_num, document_type, display_date, total_consideration,
#'   assessed_value, common_level_ratio, fair_market_value, property_count.
#' @return A tibble with typed columns added. Original character columns are
#'   retained for traceability.
parse_rtt <- function(rtt) {
  required_cols <- c("opa_account_num", "document_type", "display_date",
                     "total_consideration", "assessed_value",
                     "common_level_ratio", "fair_market_value",
                     "property_count")
  missing <- setdiff(required_cols, names(rtt))
  if (length(missing) > 0) {
    stop("parse_rtt: missing required columns: ",
         paste(missing, collapse = ", "))
  }

  rtt %>%
    mutate(
      parcel_number = as.character(opa_account_num),
      sale_date     = as.Date(substr(display_date, 1, 10)),
      year          = year(sale_date),
      month         = month(sale_date),
      quarter       = quarter(sale_date),
      sale_price    = suppressWarnings(as.numeric(total_consideration)),
      av            = suppressWarnings(as.numeric(assessed_value)),
      clr           = suppressWarnings(as.numeric(common_level_ratio)),
      fmv           = suppressWarnings(as.numeric(fair_market_value)),
      prop_count    = suppressWarnings(as.numeric(property_count))
    )
}


#' Apply the arm's-length filter to parsed RTT records.
#'
#' The filter implements the IAAO-standard exclusions plus two project-specific
#' rules. See notes/02_what_counts_as_a_sale.md for the reasoning behind each.
#'
#' Filters applied:
#'   1. document_type contains "DEED" (keeps ownership transfers; drops
#'      mortgages, satisfactions, releases, etc.)
#'   2. document_type does not contain "SHERIFF" (drops foreclosure auctions)
#'   3. sale_price >= 1000 (drops nominal-price transfers)
#'   4. av > 0 (requires a valid assessed value)
#'   5. prop_count is NA or 1 (drops multi-property deeds)
#'
#' @param rtt A tibble of parsed RTT records (output of parse_rtt).
#' @param year_min Lower bound on transaction year (inclusive).
#' @param year_max Upper bound on transaction year (inclusive).
#' @return A filtered tibble. Rows that fail any filter are dropped.
apply_arms_length_filter <- function(rtt,
                                     year_min = 2014,
                                     year_max = 2025) {
  rtt %>%
    filter(
      grepl("DEED",    document_type, ignore.case = TRUE),
      !grepl("SHERIFF", document_type, ignore.case = TRUE),
      !is.na(sale_price), sale_price >= 1000,
      !is.na(av), av > 0,
      is.na(prop_count) | prop_count == 1,
      year >= year_min, year <= year_max
    )
}


#' Deduplicate to one record per parcel per year.
#'
#' Where a parcel has multiple recorded transactions in a single year (typically
#' corrective deeds or administrative cleanups), retain only the record with the
#' highest sale_price. The dedup is performed after the arm's-length filter so
#' that filtered-out records do not influence the choice.
#'
#' @param rtt A filtered tibble of RTT records.
#' @return A deduplicated tibble with one row per (parcel_number, year),
#'   with the ratio column added.
dedup_parcel_year <- function(rtt) {
  rtt %>%
    arrange(parcel_number, year, desc(sale_price)) %>%
    distinct(parcel_number, year, .keep_all = TRUE) %>%
    mutate(ratio = av / sale_price)
}


#' Flag in-trim records using a per-year IQR rule.
#'
#' For each year, compute the 1st and 3rd quartiles of the assessment ratio
#' and flag records whose ratio falls within [Q1 - 1.5*IQR, Q3 + 1.5*IQR].
#' This follows IAAO standard practice for ratio studies.
#'
#' Important: records flagged out are NOT dropped. The in_trim flag is added
#' as a column, and analyses choose whether to scope to in-trim records or
#' study the trimmed-out distribution separately.
#'
#' @param rtt A deduplicated tibble with a ratio column.
#' @return The input tibble with an in_trim logical column added.
flag_in_trim <- function(rtt) {
  if (!"ratio" %in% names(rtt)) {
    stop("flag_in_trim: input must have a ratio column. ",
         "Call dedup_parcel_year first.")
  }

  rtt %>%
    group_by(year) %>%
    mutate(
      .q1     = quantile(ratio, 0.25, na.rm = TRUE),
      .q3     = quantile(ratio, 0.75, na.rm = TRUE),
      .iqr    = .q3 - .q1,
      in_trim = ratio >= (.q1 - 1.5 * .iqr) &
                ratio <= (.q3 + 1.5 * .iqr) &
                !is.na(ratio) & is.finite(ratio)
    ) %>%
    ungroup() %>%
    select(-.q1, -.q3, -.iqr)
}


#' Pool a set of OPA annual snapshots into a parcel-level characteristic file.
#'
#' OPA publishes annual snapshots of the property database. For analyses that
#' depend on property characteristics in the 2014-2019 window, we pool all
#' available annual files and retain the most recent record per parcel.
#'
#' Single-family residential only: category_code == 1.
#'
#' @param opa_files Character vector of file paths to OPA annual snapshots.
#' @param snap_years Integer vector matching opa_files, used to identify the
#'   most recent record per parcel.
#' @param char_cols Character vector of OPA columns to retain.
#' @return A tibble with one row per parcel containing the most recent
#'   recorded values for the requested columns.
pool_opa_snapshots <- function(opa_files,
                               snap_years,
                               char_cols = c("parcel_number", "category_code",
                                             "census_tract", "total_livable_area",
                                             "number_of_bedrooms", "number_of_bathrooms",
                                             "number_stories", "year_built",
                                             "exterior_condition", "interior_condition",
                                             "quality_grade", "geographic_ward",
                                             "zoning")) {
  if (length(opa_files) != length(snap_years)) {
    stop("pool_opa_snapshots: opa_files and snap_years must be the same length.")
  }

  pooled <- purrr::map2_dfr(opa_files, snap_years, function(path, yr) {
    readr::read_csv(path,
                    col_types = readr::cols(.default = "c"),
                    show_col_types = FALSE) %>%
      filter(category_code == "1") %>%
      select(dplyr::any_of(char_cols)) %>%
      mutate(parcel_number = as.character(parcel_number),
             opa_snap_year = yr)
  })

  pooled %>%
    arrange(parcel_number, desc(opa_snap_year)) %>%
    distinct(parcel_number, .keep_all = TRUE) %>%
    select(-opa_snap_year, -dplyr::any_of("category_code"))
}


#' Merge OPA characteristics onto a transaction tibble.
#'
#' Left-joins OPA characteristic columns onto the RTT-derived transaction
#' file by parcel_number, then arranges the output column order to match
#' the standard master-panel layout.
#'
#' @param rtt A deduplicated, trimmed RTT tibble.
#' @param opa A parcel-level OPA characteristic tibble (output of
#'   pool_opa_snapshots or a single OPA snapshot).
#' @param data_period Character label (e.g., "2014-2019" or "2020-2025") to
#'   record which build produced this slice.
#' @param source Character label describing the underlying inputs (e.g.,
#'   "RTT+OPA_snapshot" or "RTT+OPA_2020").
#' @return A tibble with the standard column order used throughout the project.
merge_opa_to_rtt <- function(rtt, opa, data_period, source) {
  rtt %>%
    left_join(opa, by = "parcel_number") %>%
    select(
      parcel_number, year, month, quarter, sale_date,
      sale_price, av, clr, fmv, ratio, in_trim,
      document_type, ward, zip_code, street_address,
      dplyr::any_of(c("census_tract", "total_livable_area",
                      "number_of_bedrooms", "number_of_bathrooms",
                      "number_stories", "year_built", "exterior_condition",
                      "interior_condition", "quality_grade",
                      "geographic_ward", "zoning"))
    ) %>%
    mutate(
      data_period = data_period,
      source      = source
    )
}


#' Harmonize column types after concatenating the two build periods.
#'
#' bind_rows can leave mixed types if one input has all-NA columns. This
#' function applies the canonical types and sorts the output.
#'
#' @param master Concatenated master tibble.
#' @return The same tibble with canonical column types and sorted by
#'   (year, parcel_number).
harmonize_master <- function(master) {
  master %>%
    mutate(
      sale_price          = as.numeric(sale_price),
      av                  = as.numeric(av),
      clr                 = as.numeric(clr),
      fmv                 = as.numeric(fmv),
      ratio               = as.numeric(ratio),
      year                = as.integer(year),
      month               = as.integer(month),
      quarter             = as.integer(quarter),
      total_livable_area  = as.numeric(total_livable_area),
      number_of_bedrooms  = as.numeric(number_of_bedrooms),
      number_of_bathrooms = as.numeric(number_of_bathrooms),
      number_stories      = as.numeric(number_stories),
      year_built          = as.numeric(year_built),
      parcel_number       = as.character(parcel_number),
      census_tract        = as.character(census_tract),
      ward                = as.character(ward),
      zip_code            = as.character(zip_code),
      in_trim             = as.logical(in_trim)
    ) %>%
    arrange(year, parcel_number)
}
