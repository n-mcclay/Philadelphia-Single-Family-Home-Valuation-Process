# ----------------------------------------------------------------------------
# analysis/01_build_master_panel.R
#
# Constructs the master analytical panel for the Philadelphia single-family
# valuation project. Reads raw OPA assessment files and Real Estate Transfer
# Tax (RTT) records from data-raw/, applies the cleaning pipeline defined
# in R/data_cleaning.R, performs a spatial join against the 2016 GMA
# boundary file, and writes two CSVs to data/processed/.
#
# Reads (from data-raw/):
#   RTT_SUMMARY_2014-2015.csv
#   RTT_SUMMARY_2016-2017.csv
#   RTT_SUMMARY_2018-2019.csv
#   real_estate_transfers.csv               (covers 2020-2025)
#   opa_property_assessment_2014.csv ... 2019.csv
#   opa_properties_public_2020.csv
#   gma_boundaries_2016.geojson
#
# Writes (to data/processed/):
#   master_philadelphia_2014_2025_with_gma.csv
#   parcel_gma_crosswalk.csv
#
# Source URLs for the raw files: see data/raw_data_sources.md.
#
# Run time: about 3-5 minutes on a recent laptop, dominated by the spatial
# join against the 580k-row parcel point file.
# ----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(vroom)
  library(lubridate)
  library(sf)
})

# Source the reusable cleaning functions
source(here::here("R", "data_cleaning.R"))


# ----------------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------------

DATA_RAW <- here::here("data-raw")
DATA_OUT <- here::here("data", "processed")
dir.create(DATA_OUT, showWarnings = FALSE, recursive = TRUE)


# ----------------------------------------------------------------------------
# Stage 1: Build 2014-2019 panel
# ----------------------------------------------------------------------------

message("Stage 1: 2014-2019 panel")

rtt_files_pre2020 <- file.path(DATA_RAW, c(
  "RTT_SUMMARY_2014-2015.csv",
  "RTT_SUMMARY_2016-2017.csv",
  "RTT_SUMMARY_2018-2019.csv"
))

needed_cols <- c("opa_account_num", "document_type", "display_date",
                 "total_consideration", "assessed_value", "common_level_ratio",
                 "fair_market_value", "property_count", "ward", "zip_code",
                 "street_address")

rtt_pre2020 <- map_dfr(
  rtt_files_pre2020,
  ~ vroom(.x,
          col_select     = all_of(needed_cols),
          col_types      = cols(.default = "c"),
          show_col_types = FALSE)
)
message("  RTT raw rows: ", format(nrow(rtt_pre2020), big.mark = ","))

rtt_pre2020 <- rtt_pre2020 %>%
  parse_rtt() %>%
  apply_arms_length_filter(year_min = 2014, year_max = 2019) %>%
  dedup_parcel_year() %>%
  flag_in_trim()

message("  After filter + dedup: ", format(nrow(rtt_pre2020), big.mark = ","))
message("  In-trim share: ",
        round(mean(rtt_pre2020$in_trim) * 100, 1), "%")

# Pool the 2014-2019 OPA snapshots and merge
opa_files_pre2020   <- file.path(DATA_RAW,
                                 sprintf("opa_property_assessment_%d.csv", 2014:2019))
opa_pool_pre2020    <- pool_opa_snapshots(opa_files_pre2020, 2014:2019)

master_pre2020 <- merge_opa_to_rtt(
  rtt        = rtt_pre2020,
  opa        = opa_pool_pre2020,
  data_period = "2014-2019",
  source     = "RTT+OPA_snapshot"
)

message("  Master 2014-2019: ", format(nrow(master_pre2020), big.mark = ","))
message("  OPA match rate: ",
        round(mean(!is.na(master_pre2020$total_livable_area)) * 100, 1), "%")


# ----------------------------------------------------------------------------
# Stage 2: Build 2020-2025 panel
# ----------------------------------------------------------------------------

message("\nStage 2: 2020-2025 panel")

rtt_post2020 <- vroom(
  file.path(DATA_RAW, "real_estate_transfers.csv"),
  col_select     = all_of(needed_cols),
  col_types      = cols(.default = "c"),
  show_col_types = FALSE
)
message("  RTT raw rows: ", format(nrow(rtt_post2020), big.mark = ","))

rtt_post2020 <- rtt_post2020 %>%
  parse_rtt() %>%
  apply_arms_length_filter(year_min = 2020, year_max = 2025) %>%
  dedup_parcel_year() %>%
  flag_in_trim()

message("  After filter + dedup: ", format(nrow(rtt_post2020), big.mark = ","))
message("  In-trim share: ",
        round(mean(rtt_post2020$in_trim) * 100, 1), "%")

# 2020-2025 uses a single OPA snapshot
opa_post2020 <- vroom(
  file.path(DATA_RAW, "opa_properties_public_2020.csv"),
  col_select = c("parcel_number", "category_code", "census_tract",
                 "total_livable_area", "number_of_bedrooms", "number_of_bathrooms",
                 "number_stories", "year_built", "exterior_condition",
                 "interior_condition", "quality_grade", "geographic_ward",
                 "zoning"),
  col_types      = cols(.default = "c"),
  show_col_types = FALSE
) %>%
  filter(category_code == "1") %>%
  mutate(parcel_number = as.character(parcel_number)) %>%
  select(-category_code)

master_post2020 <- merge_opa_to_rtt(
  rtt        = rtt_post2020,
  opa        = opa_post2020,
  data_period = "2020-2025",
  source     = "RTT+OPA_2020"
)

message("  Master 2020-2025: ", format(nrow(master_post2020), big.mark = ","))
message("  OPA match rate: ",
        round(mean(!is.na(master_post2020$total_livable_area)) * 100, 1), "%")


# ----------------------------------------------------------------------------
# Stage 3: Concatenate and harmonize
# ----------------------------------------------------------------------------

message("\nStage 3: Concatenate")

master <- bind_rows(master_pre2020, master_post2020) %>%
  harmonize_master()

message("  Combined master: ", format(nrow(master), big.mark = ","), " rows")
message("  Year range: ", min(master$year), " to ", max(master$year))

# Sanity check: every year between 2014 and 2025 should be represented
year_counts <- master %>% count(year)
if (nrow(year_counts) < 12) {
  warning("Expected 12 years of data; found ", nrow(year_counts))
}


# ----------------------------------------------------------------------------
# Stage 4: GMA crosswalk via spatial join
# ----------------------------------------------------------------------------

message("\nStage 4: GMA crosswalk")

# Parcel points from the 2020 OPA properties file. The shape column contains
# WKT point geometry with an embedded SRID prefix indicating Pennsylvania
# State Plane South (EPSG:2272), feet. Strip the prefix before parsing so
# sf reads with correct CRS metadata.
opa_full <- vroom(
  file.path(DATA_RAW, "opa_properties_public_2020.csv"),
  col_select     = c("parcel_number", "shape"),
  show_col_types = FALSE
) %>%
  filter(!is.na(shape), shape != "") %>%
  mutate(
    parcel_number = as.character(parcel_number),
    shape         = gsub("^SRID=\\d+;", "", shape)
  )

opa_pts <- st_as_sf(opa_full, wkt = "shape", crs = 2272) %>%
  select(parcel_number)

message("  Parcels with valid coordinates: ",
        format(nrow(opa_pts), big.mark = ","))

# Sanity check: the bulk of points should sit inside Philadelphia in PA
# State Plane South (feet). Expected core: x in [2.66M, 2.75M], y in
# [215k, 290k]. The check uses interior quantiles rather than min/max so
# that a single outlier parcel with a bad coordinate does not trip the
# warning.
bb_q <- list(
  x = quantile(st_coordinates(opa_pts)[, "X"], c(0.01, 0.99), na.rm = TRUE),
  y = quantile(st_coordinates(opa_pts)[, "Y"], c(0.01, 0.99), na.rm = TRUE)
)
if (bb_q$x[1] < 2.6e6 || bb_q$x[2] > 2.8e6 ||
    bb_q$y[1] < 2.0e5 || bb_q$y[2] > 3.0e5) {
  warning("Parcel coordinate distribution outside expected Philadelphia range; ",
          "check the CRS on opa_properties_public_2020.csv.")
}

# GMA boundary polygons
gmas <- st_read(
  file.path(DATA_RAW, "gma_boundaries_2016.geojson"),
  quiet = TRUE
) %>%
  st_transform(2272) %>%
  select(GMA, geometry) %>%
  mutate(zone = substr(GMA, 1, 1))

message("  GMAs in boundary file: ", nrow(gmas))
message("  Zones: ", n_distinct(gmas$zone))

# Spatial join: each parcel point gets the containing GMA polygon. A small
# number of parcels sit exactly on a GMA boundary and receive multiple
# matches from st_within. Deduplicate to one row per parcel (taking the
# first match) so that the downstream left_join is strictly one-to-many
# in the (master <- crosswalk) direction.
crosswalk <- st_join(opa_pts, gmas, join = st_within) %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(parcel_number, GMA, zone) %>%
  distinct(parcel_number, .keep_all = TRUE)

message("  Parcels matched to a GMA: ",
        format(sum(!is.na(crosswalk$GMA)), big.mark = ","),
        " (",
        round(mean(!is.na(crosswalk$GMA)) * 100, 1), "%)")


# ----------------------------------------------------------------------------
# Stage 5: Merge crosswalk into master
# ----------------------------------------------------------------------------

message("\nStage 5: Merge crosswalk into master")

master_with_gma <- master %>%
  left_join(crosswalk, by = "parcel_number")

n_master    <- nrow(master_with_gma)
n_matched   <- sum(!is.na(master_with_gma$GMA))
pct_matched <- round(n_matched / n_master * 100, 1)

message("  Master rows: ", format(n_master, big.mark = ","))
message("  Matched to GMA: ",
        format(n_matched, big.mark = ","),
        " (", pct_matched, "%)")

# Sanity check: match rate should be high and stable across years
match_by_year <- master_with_gma %>%
  group_by(year) %>%
  summarise(
    pct_matched = mean(!is.na(GMA)) * 100,
    .groups = "drop"
  )
if (any(match_by_year$pct_matched < 90)) {
  warning("GMA match rate fell below 90% in at least one year; ",
          "check that opa_properties_public_2020.csv covers historical parcels.")
}


# ----------------------------------------------------------------------------
# Stage 6: Write outputs
# ----------------------------------------------------------------------------

message("\nStage 6: Write outputs")

write_csv(master_with_gma,
          file.path(DATA_OUT, "master_philadelphia_2014_2025_with_gma.csv"))
write_csv(crosswalk,
          file.path(DATA_OUT, "parcel_gma_crosswalk.csv"))

message("  Wrote master_philadelphia_2014_2025_with_gma.csv (",
        format(nrow(master_with_gma), big.mark = ","), " rows)")
message("  Wrote parcel_gma_crosswalk.csv (",
        format(nrow(crosswalk), big.mark = ","), " rows)")
message("\nDone.")
