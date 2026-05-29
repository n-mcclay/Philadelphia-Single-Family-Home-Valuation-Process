# ----------------------------------------------------------------------------
# analysis/06_tract_quarter_uncertainty.R
#
# Tract-quarter panel of within-tract hedonic-residual dispersion, with an
# interrupted time series fit around three institutional events: the tax
# year 2019 reassessment, the tax year 2023 reassessment (the first
# post-pandemic reassessment, accompanied by the implementation of OPA's
# CAMA system and the creation of a sales validation unit), and the tax
# year 2025 reassessment (the most recent event in the panel).
#
# What this analysis can and cannot identify
# ============================================================================
# A hedonic-residual outcome measures within-tract market pricing
# heterogeneity conditional on observed characteristics. The 2019, 2023,
# and 2025 reassessments did not change sale prices or property
# characteristics. They changed assessed values. So this analysis cannot
# causally identify the effect of any reassessment on assessment quality.
# What it can identify is whether within-tract pricing heterogeneity
# shifted around the timing of each reassessment.
#
# A second caveat is institutional: each of these reassessments was
# bundled with other administrative changes (CAMA implementation, the
# creation of the sales validation unit, an external data-collection
# contract, methodology revisions). What the ITS detects, if anything, is
# a joint effect of the package of changes that surrounded a given event,
# not a clean parameter shift in a stable system.
#
# With those caveats, the ITS documents the observed time-series pattern
# of within-tract pricing heterogeneity across the 2014-2025 period and
# tests whether observable shifts coincide with these events.
# ============================================================================
#
# Reads:
#   data/processed/master_philadelphia_2014_2025_with_gma.csv
#
# Writes:
#   data/processed/tract_quarter_panel.csv      -- tract-quarter outcomes.
#   data/processed/its_results.csv              -- ITS coefficients.
#   figures/fig_10_tract_quarter_trajectory.pdf -- aggregate trajectory plot.
#   figures/fig_11_its_breaks.pdf               -- breaks visualization.
# ----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(tibble)
  library(fixest)
})

source(here::here("R", "plotting.R"))


# ----------------------------------------------------------------------------
# Load and filter to complete hedonic sample
# ----------------------------------------------------------------------------

master <- read_csv(
  here::here("data", "processed",
             "master_philadelphia_2014_2025_with_gma.csv"),
  show_col_types = FALSE
)

# Hedonic sample: complete characteristics required. This implicitly
# restricts to single-family residential because OPA only populates
# structural fields for that category.
hed_vars <- c("total_livable_area", "number_of_bedrooms", "number_of_bathrooms",
              "number_stories", "year_built", "exterior_condition",
              "interior_condition", "quality_grade", "census_tract")

hed_sample <- master %>%
  filter(
    !is.na(sale_price), sale_price > 0,
    if_all(all_of(hed_vars), ~ !is.na(.)),
    total_livable_area > 0,
    year_built >= 1700, year_built <= 2025
  ) %>%
  mutate(
    log_sp           = log(sale_price),
    log_area         = log(total_livable_area),
    age              = year - year_built,
    age2             = age^2,
    year_quarter     = year + (quarter - 1) / 4,
    tract_quarter_id = paste0(census_tract, "_", year, "Q", quarter)
  )

message("Hedonic sample: ", format(nrow(hed_sample), big.mark = ","),
        " transactions across ", n_distinct(hed_sample$census_tract),
        " census tracts and ",
        n_distinct(paste(hed_sample$year, hed_sample$quarter)),
        " quarters")


# ----------------------------------------------------------------------------
# Fit the hedonic
# ----------------------------------------------------------------------------
#
# Specification: log(sale_price) on structural and condition characteristics
# plus quarter and tract fixed effects. The data_period term flexes the
# intercept across the OPA snapshot seam (pooled 2014-2019 vs. current-file
# 2020-2025); see notes/02 for the institutional context.
#
# fixest::feols absorbs the high-dimensional FEs without dummy expansion.

message("\nFitting hedonic regression")

hedonic <- feols(
  log_sp ~ log_area + number_of_bedrooms + number_of_bathrooms +
           number_stories + age + age2 +
           i(exterior_condition) + i(interior_condition) +
           i(quality_grade) + i(data_period)
  | year_quarter + census_tract,
  data = hed_sample
)

# Summary stats on the hedonic
message("  N: ", format(nobs(hedonic), big.mark = ","))
message("  Within-R2: ", sprintf("%.3f", fitstat(hedonic, "wr2")$wr2))
message("  Adj-R2:    ", sprintf("%.3f", fitstat(hedonic, "ar2")$ar2))

# Extract residuals
hed_sample$resid <- residuals(hedonic)


# ----------------------------------------------------------------------------
# Build the tract-quarter panel
# ----------------------------------------------------------------------------
#
# Outcome: IQR of hedonic residuals within each tract-quarter cell, for
# cells with at least 5 transactions. The IQR is preferred to SD here
# because the residual distribution has thicker tails than Gaussian.

message("\nBuilding tract-quarter panel")

tract_quarter <- hed_sample %>%
  group_by(census_tract, year, quarter) %>%
  summarise(
    n          = n(),
    median_sp  = median(sale_price),
    median_res = median(resid),
    iqr_res    = as.numeric(quantile(resid, 0.75) - quantile(resid, 0.25)),
    sd_res     = sd(resid),
    .groups    = "drop"
  ) %>%
  filter(n >= 5) %>%
  mutate(
    year_quarter = year + (quarter - 1) / 4,
    time_index   = (year - 2014) * 4 + (quarter - 1)  # 0, 1, 2, ... 47
  )

message("  Tract-quarter cells (n >= 5): ",
        format(nrow(tract_quarter), big.mark = ","))
message("  Distinct tracts: ", n_distinct(tract_quarter$census_tract))
message("  Mean cells per tract: ",
        round(nrow(tract_quarter) / n_distinct(tract_quarter$census_tract), 1))
message("  Mean IQR of residuals: ",
        round(mean(tract_quarter$iqr_res), 3))

write_csv(tract_quarter,
          here::here("data", "processed", "tract_quarter_panel.csv"))


# ----------------------------------------------------------------------------
# Aggregate trajectory (descriptive)
# ----------------------------------------------------------------------------
#
# Median across tracts of the tract-quarter IQR. Shows the citywide
# trajectory of within-tract pricing heterogeneity, with reference lines
# at the three reassessment effective dates.

agg_trajectory <- tract_quarter %>%
  group_by(year_quarter) %>%
  summarise(
    n_tracts     = n(),
    median_iqr   = median(iqr_res),
    p25_iqr      = quantile(iqr_res, 0.25),
    p75_iqr      = quantile(iqr_res, 0.75),
    .groups      = "drop"
  )

print(head(agg_trajectory, 10))

reassessment_dates <- tibble(
  event_date = c(2019.0, 2023.0, 2025.0),
  event_name = c("TY2019\nreassessment",
                 "TY2023\nreassessment",
                 "TY2025\nreassessment")
)

fig10 <- ggplot(agg_trajectory, aes(year_quarter, median_iqr)) +
  geom_ribbon(aes(ymin = p25_iqr, ymax = p75_iqr),
              fill = "grey85", alpha = 0.6) +
  geom_line(color = PAL$blue, linewidth = 0.7) +
  geom_point(color = PAL$blue, size = 1.4) +
  geom_vline(data = reassessment_dates,
             aes(xintercept = event_date),
             linetype = "dashed", color = PAL$gray) +
  geom_text(data = reassessment_dates,
            aes(x = event_date, y = max(agg_trajectory$p75_iqr) * 1.02,
                label = event_name),
            hjust = -0.05, vjust = 1, size = 2.8, color = PAL$gray,
            lineheight = 0.85) +
  scale_x_continuous(breaks = seq(2014, 2025, by = 2)) +
  labs(
    title    = "Within-tract hedonic-residual dispersion, 2014-2025",
    subtitle = "Median across tracts of the IQR of hedonic residuals; shaded band is 25th-75th percentile",
    x        = NULL,
    y        = "IQR of log(sale price) residuals within tract-quarter",
    caption  = paste0("Source: Author's analysis. Hedonic residuals from a tract- and quarter-FE model. ",
                      "Dashed lines mark reassessment effective dates.\n",
                      "Note: this outcome measures market pricing heterogeneity; it does not directly ",
                      "measure assessment accuracy.")
  ) +
  theme_project()

save_figure(fig10, "fig_10_tract_quarter_trajectory.pdf",
            orientation = "landscape")

message("\n  Wrote figures/fig_10_tract_quarter_trajectory.pdf")


# ----------------------------------------------------------------------------
# Interrupted time series with three breaks
# ----------------------------------------------------------------------------
#
# Specification per tract-quarter observation:
#   iqr_res = alpha_t + beta0 * time
#                     + beta1 * post_2019Q1 + beta2 * post_2019Q1 * time_since_2019Q1
#                     + beta3 * post_2023Q1 + beta4 * post_2023Q1 * time_since_2023Q1
#                     + beta5 * post_2025Q1 + beta6 * post_2025Q1 * time_since_2025Q1
#                     + tract_FE + epsilon
#
# Each break adds a level shift (beta_post) and a slope change
# (beta_post*time_since).
#
# Standard errors: two-way clustered, by census_tract AND by year_quarter.
#   - Clustering by census_tract handles arbitrary temporal correlation within
#     each tract's time series (the standard panel-data concern).
#   - Clustering by year_quarter handles arbitrary spatial correlation across
#     tracts within the same quarter. This is motivated by analysis/05, which
#     documented substantial spatial autocorrelation among GMAs (SAR rho =
#     0.339); the same dynamics almost certainly produce spatial structure
#     among tracts. Single-dimension clustering on tract alone, our original
#     choice, was inconsistent with what analysis/05 had already shown about
#     this data. The two-way version brings analysis/06's inferential
#     standard up to where analysis/05 had set it.
#
# Two-way clustering is a non-parametric correction: it does not model the
# spatial structure, only acknowledges it. A fully spatial-panel estimator
# (e.g., spatial Durbin or splm::spml) would do more, but at substantially
# greater methodological cost and modest substantive payoff given how large
# the t-statistics here already are. See notes/06 for further discussion.

message("\nFitting three-break ITS")

# Define the three break points in time_index units
# 2019Q1 corresponds to time_index = (2019-2014)*4 + 0 = 20
# 2023Q1 corresponds to time_index = (2023-2014)*4 + 0 = 36
# 2025Q1 corresponds to time_index = (2025-2014)*4 + 0 = 44
break_2019 <- 20
break_2023 <- 36
break_2025 <- 44

its_data <- tract_quarter %>%
  mutate(
    time              = time_index,
    post_2019         = as.integer(time_index >= break_2019),
    post_2023         = as.integer(time_index >= break_2023),
    post_2025         = as.integer(time_index >= break_2025),
    time_since_2019   = pmax(0, time_index - break_2019),
    time_since_2023   = pmax(0, time_index - break_2023),
    time_since_2025   = pmax(0, time_index - break_2025)
  )

its_fit <- feols(
  iqr_res ~ time +
            post_2019 + time_since_2019 +
            post_2023 + time_since_2023 +
            post_2025 + time_since_2025
  | census_tract,
  data    = its_data,
  cluster = ~ census_tract + year_quarter
)

print(summary(its_fit))


# ----------------------------------------------------------------------------
# Save coefficient table
# ----------------------------------------------------------------------------

its_table <- as_tibble(broom::tidy(its_fit, conf.int = TRUE))
print(its_table)

write_csv(its_table,
          here::here("data", "processed", "its_results.csv"))


# ----------------------------------------------------------------------------
# Figure 11: ITS breaks visualization
# ----------------------------------------------------------------------------
#
# Predicted citywide trajectory under the ITS model, overlaid on the
# observed aggregate trajectory. Vertical lines mark the three breaks.

# Build a citywide-trajectory prediction by setting up a single-tract
# template and predicting through it, then averaging predictions across
# tracts in the actual data.
its_data$predicted <- predict(its_fit, newdata = its_data)

agg_predicted <- its_data %>%
  group_by(year_quarter) %>%
  summarise(
    observed_median  = median(iqr_res),
    predicted_median = median(predicted),
    .groups = "drop"
  )

fig11 <- ggplot(agg_predicted, aes(year_quarter)) +
  geom_point(aes(y = observed_median, color = "Observed"),
             size = 1.6) +
  geom_line(aes(y = predicted_median, color = "ITS prediction"),
            linewidth = 0.7) +
  geom_vline(data = reassessment_dates, aes(xintercept = event_date),
             linetype = "dashed", color = PAL$gray) +
  geom_text(data = reassessment_dates,
            aes(x = event_date,
                y = max(agg_predicted$observed_median) * 1.02,
                label = event_name),
            hjust = -0.05, vjust = 1, size = 2.8, color = PAL$gray,
            lineheight = 0.85) +
  scale_color_manual(values = c("Observed"        = PAL$blue,
                                "ITS prediction"  = PAL$red),
                     name = NULL) +
  scale_x_continuous(breaks = seq(2014, 2025, by = 2)) +
  labs(
    title    = "Interrupted time series fit with three institutional breaks",
    subtitle = "Observed and ITS-predicted citywide median of tract-quarter residual IQR",
    x        = NULL,
    y        = "IQR of log(sale price) residuals",
    caption  = paste0("Source: Author's analysis. ITS fit at the tract-quarter level with tract FE ",
                      "and two-way clustered standard errors (tract \u00d7 year-quarter).\n",
                      "Breaks at 2019Q1, 2023Q1, and 2025Q1 correspond to reassessment effective dates.")
  ) +
  theme_project() +
  theme(legend.position = "bottom")

save_figure(fig11, "fig_11_its_breaks.pdf", orientation = "landscape")

message("  Wrote figures/fig_11_its_breaks.pdf")


# ----------------------------------------------------------------------------
# Summary numbers
# ----------------------------------------------------------------------------

message("\nKey numbers:")
message("  Hedonic transactions:     ", format(nobs(hedonic), big.mark = ","))
message("  Hedonic adj-R2:            ", sprintf("%.3f", fitstat(hedonic, "ar2")$ar2))
message("  Tract-quarter cells:       ",
        format(nrow(tract_quarter), big.mark = ","))
message("  Distinct tracts:           ", n_distinct(tract_quarter$census_tract))
message("  Mean residual IQR:         ",
        sprintf("%.3f", mean(tract_quarter$iqr_res)))
message("  2019Q1 level shift:        ",
        sprintf("%.4f", coef(its_fit)["post_2019"]))
message("  2023Q1 level shift:        ",
        sprintf("%.4f", coef(its_fit)["post_2023"]))
message("  2025Q1 level shift:        ",
        sprintf("%.4f", coef(its_fit)["post_2025"]))

message("\nDone.")
