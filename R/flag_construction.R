# ----------------------------------------------------------------------------
# R/flag_construction.R
#
# Construction of transaction-level data-quality flags.
#
# ============================================================================
# A note on what this file does and does not include
# ============================================================================
#
# An earlier version of this project (the Valuation Uncertainty paper, late
# 2025) constructed a flag called extreme_ratio_flag, defined as:
#
#     extreme_ratio_flag = (sp/fmv < 0.5) | (sp/fmv > 1.5)
#
# This flag was then bundled into a composite "data quality" indicator and
# used as a predictor in analyses where dispersion of the assessment ratio
# was the outcome. The flag is constructed from the assessment ratio itself
# (sp/fmv is a transformation of the assessment ratio, and av/sale_price is
# its IAAO-standard reciprocal). Using it to predict ratio behavior is
# tautological: extreme ratios predict extreme ratios.
#
# This file deliberately does NOT include any flag derived from the ratio,
# from sale_price/fmv, from av/sale_price, or from any function thereof.
# The flags below describe properties of the administrative record itself
# (date missing, characteristic missing, value implausible on its own terms)
# rather than properties of the ratio. They can be used as predictors in
# analyses where the ratio or its dispersion is the outcome.
#
# This is the central methodological correction motivating the rebuild of
# this project. See notes/03_the_circular_flag.md for the longer story.
#
# ============================================================================
#
# The flag taxonomy below distinguishes three categories:
#
#   1. Record-completeness flags: is a required field present and plausible?
#      (bad_year_built_flag, bad_sqft_flag)
#
#   2. Internal-consistency flags: are values consistent with each other,
#      net of administrative policy?
#      (fmv_identity_flag, room_logic_flag)
#
#   3. Price-pattern flags: does the recorded sale_price have a pattern
#      suggestive of a non-arm's-length or administrative transfer?
#      Critically, these flags are constructed from sale_price ALONE and
#      do not reference assessed value, fair market value, or any ratio.
#      (nominal_price_flag, round_price_flag)
#
# An earlier version of this file included three flags that proved to be
# dead in the working sample because the arm's-length filter (see
# R/data_cleaning.R) already enforces the conditions they were designed
# to catch: bad_date_flag, bad_av_flag, and big_date_gap_flag. They have
# been removed. If you bypass the arm's-length filter for diagnostic
# purposes and want to flag these conditions, reintroduce them locally.
#
# A note on fmv_identity_flag: Philadelphia's Common Level Ratio (CLR) was
# set to 1.0 during the AVI implementation period (tax years 2014-2019),
# which makes av == fmv a mechanical identity for those years rather than
# a data-quality concern. The flag fires only when av == fmv AND clr != 1.0,
# isolating genuine field-copy errors from the period when av and fmv were
# definitionally equal.
#
# Any future flag added to this file must be classifiable into one of these
# three categories. If a proposed flag would require referencing the ratio
# or any derivative of it, it does not belong here.
# ----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})


#' Construct the standard flag set.
#'
#' Takes a transaction-level tibble produced by the data_cleaning.R pipeline
#' and adds a set of logical flag columns plus an n_flags integer counting
#' how many fired for each transaction.
#'
#' The flags are independent of the assessment ratio by construction.
#' See the file header for the reasoning behind this constraint and for the
#' history of flags that were removed because they proved dead in the
#' working sample.
#'
#' @param df A tibble with at least these columns: sale_price, av, fmv, clr,
#'   year_built, total_livable_area, number_of_bedrooms,
#'   number_of_bathrooms.
#' @return The input tibble with flag columns and n_flags added.
construct_flags <- function(df) {
  
  # Record-completeness flags
  df <- df %>%
    mutate(
      bad_year_built_flag  = is.na(year_built) | year_built < 1700 | year_built > year(today()),
      bad_sqft_flag        = is.na(total_livable_area) | total_livable_area <= 0
    )
  
  # Internal-consistency flags
  #
  # fmv_identity_flag: fair market value exactly equal to assessed value
  #   AND CLR is not 1.0. Philadelphia's CLR was set to 1.0 during the AVI
  #   period (tax years 2014-2019), making av == fmv a mechanical identity
  #   in that window rather than a data-quality concern. The CLR != 1.0
  #   restriction isolates genuine field-copy errors from this mechanical
  #   identity period.
  #
  # room_logic_flag: more bathrooms than bedrooms by more than 1, or zero
  #   of both with positive livable area. Catches a common data-entry pattern.
  df <- df %>%
    mutate(
      fmv_identity_flag = !is.na(av) & !is.na(fmv) & av == fmv & av > 0 &
        !is.na(clr) & clr != 1.0,
      room_logic_flag   = (!is.na(number_of_bedrooms) &
                             !is.na(number_of_bathrooms) &
                             number_of_bathrooms > number_of_bedrooms + 1) |
        (!is.na(number_of_bedrooms) &
           !is.na(number_of_bathrooms) &
           number_of_bedrooms == 0 &
           number_of_bathrooms == 0 &
           !is.na(total_livable_area) &
           total_livable_area > 200)
    )
  
  # Price-pattern flags
  #
  # nominal_price_flag: sale_price below $1,000. Note: the arm's-length filter
  #   already drops these; this flag exists so that if you re-introduce them
  #   for diagnostic work, you can mark them.
  #
  # round_price_flag: sale_price is suspiciously round (a multiple of $50,000
  #   below $300k, or a multiple of $100,000 above). Round prices are
  #   over-represented in non-arm's-length transfers and administrative
  #   adjustments.
  df <- df %>%
    mutate(
      nominal_price_flag = !is.na(sale_price) & sale_price < 1000,
      round_price_flag   = !is.na(sale_price) &
        ((sale_price <  300000 & sale_price %% 50000  == 0) |
           (sale_price >= 300000 & sale_price %% 100000 == 0))
    )
  
  # Total flag count, excluding the nominal_price_flag because the arm's-length
  # filter has already dropped those rows in normal use. Including it in the
  # composite would double-penalize records that survive the filter for other
  # reasons.
  df %>%
    mutate(
      n_flags = bad_year_built_flag + bad_sqft_flag +
        fmv_identity_flag + room_logic_flag + round_price_flag
    )
}


#' List the flag column names produced by construct_flags.
#'
#' Convenience function for downstream analyses that need to iterate over
#' all the flags (correlation matrices, fire-rate tables, etc.).
#'
#' @param include_nominal Logical; whether to include nominal_price_flag in
#'   the returned vector. Defaults to FALSE because the arm's-length filter
#'   removes those rows.
#' @return Character vector of flag column names.
flag_names <- function(include_nominal = FALSE) {
  base <- c(
    "bad_year_built_flag", "bad_sqft_flag",
    "fmv_identity_flag", "room_logic_flag",
    "round_price_flag"
  )
  if (include_nominal) c(base, "nominal_price_flag") else base
}