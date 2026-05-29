# ----------------------------------------------------------------------------
# analysis/05_spatial_lag.R
#
# Spatial-lag extension of the GMA heterogeneity regression. Tests whether
# outlier concentration co-varies with the same outcome in neighboring GMAs
# after conditioning on within-GMA covariates and zone fixed effects.
#
# Substantive question: are GMAs independent valuation universes, or does
# the assessor's performance in one neighborhood depend on what is happening
# in adjacent ones? The latter has implications for both the appraisal
# model's geographic structure and the interpretation of the main result
# in analysis/04.
#
# Three estimators are reported in sequence:
#   1. Moran's I on OLS residuals from the main regression -- diagnostic.
#   2. OLS with neighbor-mean pct_excl as an additional regressor --
#      transparent but biased because of simultaneity.
#   3. Maximum-likelihood spatial autoregressive (SAR) model -- the
#      textbook-correct estimator.
#
# Reads:
#   data/processed/gma_panel_2014_2017.csv (from analysis/04)
#   data-raw/gma_boundaries_2016.geojson
#
# Writes:
#   data/processed/spatial_results.csv     -- coefficient comparison.
#   figures/fig_08_spatial_lag_scatter.pdf -- own vs. neighbor pct_excl.
#   figures/fig_09_spatial_residuals_map.pdf -- map of residuals from
#                                              the non-spatial main model.
# ----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(tibble)
  library(sf)
  library(spdep)
  library(spatialreg)
  library(sandwich)
  library(lmtest)
})

source(here::here("R", "plotting.R"))


# ----------------------------------------------------------------------------
# Load the GMA panel and the boundary file
# ----------------------------------------------------------------------------

gma_panel <- read_csv(
  here::here("data", "processed", "gma_panel_2014_2017.csv"),
  show_col_types = FALSE
)

gmas_sf <- st_read(
  here::here("data-raw", "gma_boundaries_2016.geojson"),
  quiet = TRUE
) %>%
  st_transform(2272) %>%
  select(GMA)

# Inner-join boundaries to the regression panel, preserving spatial geometry.
gma_geo <- gmas_sf %>%
  inner_join(gma_panel, by = "GMA")

message("GMAs in spatial regression sample: ", nrow(gma_geo))


# ----------------------------------------------------------------------------
# Spatial weights matrix: queen contiguity, row-standardized
# ----------------------------------------------------------------------------

# Queen contiguity treats GMAs as neighbors if they share any boundary point
# (including corners). This is the standard default for areal data.
nb <- poly2nb(gma_geo, queen = TRUE)

# Some GMAs may be islands -- no neighbors among those that survived the
# n >= 20 filter from analysis/04. We allow this, but spdep needs the
# zero.policy flag to construct the weights matrix anyway.
n_islands <- sum(card(nb) == 0)
if (n_islands > 0) {
  message("  Note: ", n_islands, " GMAs are spatial islands ",
          "(no panel neighbors). They contribute to the regression but not ",
          "to spatial smoothing.")
}

W <- nb2listw(nb, style = "W", zero.policy = TRUE)


# ----------------------------------------------------------------------------
# Compute neighbor-mean pct_excl as a column on the panel
# ----------------------------------------------------------------------------

# spdep::lag.listw applies the spatial weights to a vector and returns the
# spatially-lagged value. Islands return NA because the row sum is zero.
gma_geo$lag_pct_excl <- lag.listw(W, gma_geo$pct_excl, zero.policy = TRUE)


# ----------------------------------------------------------------------------
# Step 1: Moran's I on residuals from the non-spatial main regression
# ----------------------------------------------------------------------------
#
# Refit the main regression from analysis/04, then test the residuals for
# spatial autocorrelation. If Moran's I is significant, the covariates do
# not absorb the spatial structure and a spatial specification is warranted.

message("\nStep 1: Moran's I on residuals from the non-spatial model")

main_fit <- lm(
  pct_excl ~ p90p10 + log_median_sp + factor(zone),
  data = as.data.frame(gma_geo)
)

# Drop islands for the Moran test (they have no neighbors to compare to).
panel_for_moran <- gma_geo[card(nb) > 0, ]
nb_no_islands   <- poly2nb(panel_for_moran, queen = TRUE)
W_no_islands    <- nb2listw(nb_no_islands, style = "W", zero.policy = TRUE)

moran_resid <- moran.test(
  residuals(main_fit)[card(nb) > 0],
  W_no_islands,
  zero.policy = TRUE
)

print(moran_resid)


# ----------------------------------------------------------------------------
# Step 2: OLS with the spatial lag as an additional regressor
# ----------------------------------------------------------------------------
#
# This specification adds neighbor-mean pct_excl directly to the regression.
# Easy to read but biased: own outcome depends on neighbors' outcomes which
# in turn depend on own outcome. The estimate is conditionally informative
# (sign and significance) but the magnitude is not the right effect size.

message("\nStep 2: OLS with spatial lag as a regressor (BIASED estimator)")

ols_lag <- lm(
  pct_excl ~ p90p10 + log_median_sp + lag_pct_excl + factor(zone),
  data = as.data.frame(gma_geo)
)

ols_lag_se <- coeftest(ols_lag, vcov = vcovHC(ols_lag, type = "HC3"))
print(ols_lag_se)


# ----------------------------------------------------------------------------
# Step 3: Maximum-likelihood SAR
# ----------------------------------------------------------------------------
#
# pct_excl = rho * W * pct_excl + X*beta + epsilon
#
# Estimated by maximum likelihood. The rho coefficient is the spatial
# autoregressive parameter -- the proper measure of conditional spatial
# co-movement. It bounds in (-1, 1) for a row-standardized W.

message("\nStep 3: Maximum-likelihood SAR")

sar_fit <- lagsarlm(
  pct_excl ~ p90p10 + log_median_sp + factor(zone),
  data        = as.data.frame(gma_geo),
  listw       = W,
  zero.policy = TRUE
)

print(summary(sar_fit))


# ----------------------------------------------------------------------------
# Assemble a comparison table
# ----------------------------------------------------------------------------

# Extract coefficients of interest from each model.
tidy_main <- as.data.frame(unclass(
  coeftest(main_fit, vcov = vcovHC(main_fit, type = "HC3"))
)) %>%
  rownames_to_column("term") %>%
  filter(term %in% c("p90p10", "log_median_sp")) %>%
  rename(estimate = Estimate, std_error = `Std. Error`,
         t_value = `t value`, p_value = `Pr(>|t|)`) %>%
  mutate(model = "non_spatial", .before = 1)

tidy_ols_lag <- as.data.frame(unclass(ols_lag_se)) %>%
  rownames_to_column("term") %>%
  filter(term %in% c("p90p10", "log_median_sp", "lag_pct_excl")) %>%
  rename(estimate = Estimate, std_error = `Std. Error`,
         t_value = `t value`, p_value = `Pr(>|t|)`) %>%
  mutate(model = "ols_with_spatial_lag", .before = 1)

# SAR rho is reported separately by spatialreg::summary.sarlm
sar_coefs <- summary(sar_fit)$Coef
tidy_sar <- as.data.frame(sar_coefs) %>%
  rownames_to_column("term") %>%
  filter(term %in% c("p90p10", "log_median_sp")) %>%
  rename(estimate = Estimate, std_error = `Std. Error`,
         t_value = `z value`, p_value = `Pr(>|z|)`) %>%
  mutate(model = "sar_ml", .before = 1)

# Add the rho coefficient as a separate row
tidy_rho <- tibble(
  model     = "sar_ml",
  term      = "rho",
  estimate  = sar_fit$rho,
  std_error = sar_fit$rho.se,
  t_value   = sar_fit$rho / sar_fit$rho.se,
  p_value   = 2 * (1 - pnorm(abs(sar_fit$rho / sar_fit$rho.se)))
)

spatial_results <- bind_rows(tidy_main, tidy_ols_lag, tidy_sar, tidy_rho)
print(spatial_results)

write_csv(spatial_results,
          here::here("data", "processed", "spatial_results.csv"))


# ----------------------------------------------------------------------------
# Figure 8: own pct_excl versus neighbor-mean pct_excl (Moran-style scatter)
# ----------------------------------------------------------------------------

fig8 <- gma_geo %>%
  st_drop_geometry() %>%
  filter(!is.na(lag_pct_excl)) %>%
  ggplot(aes(pct_excl * 100, lag_pct_excl * 100)) +
  geom_point(alpha = 0.45, color = PAL$blue, size = 1.4) +
  geom_smooth(method = "lm", se = TRUE,
              color = PAL$black, fill = "grey85", linewidth = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = PAL$gray) +
  labs(
    title    = "Outlier concentration co-varies with neighbors",
    subtitle = sprintf(
      "Each point is one GMA, 2014-2017. SAR rho = %.3f.",
      sar_fit$rho
    ),
    x       = "Own GMA: share with extreme ratio (%)",
    y       = "Neighbor mean: share with extreme ratio (%)",
    caption = "Source: Author's analysis. Dashed line is identity (own = neighbor mean)."
  ) +
  theme_project()

save_figure(fig8, "fig_08_spatial_lag_scatter.pdf",
            orientation = "landscape")

message("\n  Wrote figures/fig_08_spatial_lag_scatter.pdf")


# ----------------------------------------------------------------------------
# Figure 9: map of residuals from the non-spatial main model
# ----------------------------------------------------------------------------

gma_geo$resid_main <- residuals(main_fit)

fig9 <- gma_geo %>%
  ggplot() +
  geom_sf(aes(fill = resid_main * 100), color = "white", linewidth = 0.1) +
  scale_fill_gradient2(
    low = PAL$red, mid = "white", high = PAL$blue,
    midpoint = 0,
    name = "Residual\n(pp)"
  ) +
  labs(
    title    = "Residuals from the non-spatial regression",
    subtitle = "Blue: predicted lower outlier share than observed. Red: higher.",
    caption  = paste0(
      "Source: Author's analysis. Visible clustering of same-colored ",
      "polygons motivates the spatial specification."
    )
  ) +
  theme_project() +
  theme(axis.text  = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

save_figure(fig9, "fig_09_spatial_residuals_map.pdf", orientation = "square")

message("  Wrote figures/fig_09_spatial_residuals_map.pdf")


# ----------------------------------------------------------------------------
# Summary numbers
# ----------------------------------------------------------------------------

message("\nKey numbers:")
message("  GMAs in spatial sample: ", nrow(gma_geo))
message("  Spatial islands (no neighbors): ", n_islands)
message("  Moran's I on main-model residuals: ",
        sprintf("%.3f", moran_resid$estimate["Moran I statistic"]),
        " (p = ",
        sprintf("%.4f", moran_resid$p.value), ")")
message("  SAR rho: ",
        sprintf("%.3f", sar_fit$rho),
        "  (SE: ", sprintf("%.3f", sar_fit$rho.se), ")")
message("  SAR beta on p90p10: ",
        sprintf("%.4f", sar_coefs["p90p10", "Estimate"]),
        "  (compare non-spatial: ",
        sprintf("%.4f", coef(main_fit)["p90p10"]), ")")
message("  SAR beta on log_median_sp: ",
        sprintf("%.4f", sar_coefs["log_median_sp", "Estimate"]),
        "  (compare non-spatial: ",
        sprintf("%.4f", coef(main_fit)["log_median_sp"]), ")")

message("\nDone.")