# ----------------------------------------------------------------------------
# analysis/04_gma_heterogeneity.R
#
# The headline regression for the Fixed Boundaries paper. Tests whether
# within-GMA price heterogeneity predicts the concentration of extreme
# assessment ratios, controlling for price level and zone fixed effects.
#
# Substantive question: is regressivity a poor-neighborhoods phenomenon
# (driven by low prices) or a heterogeneous-neighborhoods phenomenon
# (driven by within-unit price variance)? The two have different policy
# implications.
#
# Specification:
#   pct_excl ~ p90p10 + log(median_sp) + factor(zone)
#   with HC3 heteroskedasticity-robust standard errors.
#
# Sample: GMAs in the 2014-2017 IAAO sales window (the window the City
# Controller's office used to assess the tax year 2019 reassessment),
# restricted to GMAs with at least 20 arm's-length transactions.
#
# Reads:
#   data/processed/master_philadelphia_2014_2025_with_gma.csv
#
# Writes:
#   data/processed/gma_panel_2014_2017.csv     -- GMA-level analytical file.
#   data/processed/gma_regression_results.csv  -- main + falsification coefs.
#   figures/fig_06_gma_heterogeneity_scatter.pdf  -- headline scatter.
#   figures/fig_07_gma_price_falsification.pdf    -- price-as-falsification.
# ----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(tibble)
  library(scales)
  library(sandwich)
  library(lmtest)
})

source(here::here("R", "iaao_metrics.R"))
source(here::here("R", "plotting.R"))


# ----------------------------------------------------------------------------
# Load and scope to the IAAO sales window
# ----------------------------------------------------------------------------

master <- read_csv(
  here::here("data", "processed",
             "master_philadelphia_2014_2025_with_gma.csv"),
  show_col_types = FALSE
)

window_data <- master %>%
  filter(year >= 2014, year <= 2017,
         !is.na(GMA), !is.na(zone),
         !is.na(sale_price), sale_price > 0,
         !is.na(ratio), is.finite(ratio))

message("2014-2017 arm's-length sample: ",
        format(nrow(window_data), big.mark = ","), " transactions")
message("GMAs with at least one transaction: ",
        n_distinct(window_data$GMA))


# ----------------------------------------------------------------------------
# Aggregate to GMA level
# ----------------------------------------------------------------------------
#
# Two samples are used inside each GMA:
#   - Sale-price aggregates (median_sp, p10, p90, p90p10, p_iqr) are computed
#     on the full arm's-length sample because they describe the local
#     transaction market.
#   - Ratio-based statistics (COD, median_ratio) are computed on the in-trim
#     subsample, following IAAO standard practice.
#   - pct_excl is computed on the full arm's-length sample because the whole
#     point of the outcome is to measure out-of-range transactions.

gma_panel <- window_data %>%
  group_by(GMA, zone) %>%
  summarise(
    n            = n(),
    pct_excl     = mean(ratio < 0.5 | ratio > 2.0),
    median_sp    = median(sale_price),
    p10          = quantile(sale_price, 0.10),
    p90          = quantile(sale_price, 0.90),
    p90p10       = p90 / p10,
    p_iqr        = p_iqr_fn(sale_price),
    cod          = cod_fn(ratio[in_trim]),
    median_ratio = median(ratio[in_trim], na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  mutate(log_median_sp = log(median_sp))

# Sample restriction: GMAs with at least 20 arm's-length transactions
gma_panel <- gma_panel %>% filter(n >= 20)

message("\nGMAs in regression sample (n >= 20): ", nrow(gma_panel))
message("Mean pct_excl: ", round(mean(gma_panel$pct_excl) * 100, 1), "%")
message("Mean p90/p10:  ", round(mean(gma_panel$p90p10), 2))

write_csv(gma_panel,
          here::here("data", "processed", "gma_panel_2014_2017.csv"))


# ----------------------------------------------------------------------------
# Main regression: heterogeneity hypothesis
# ----------------------------------------------------------------------------
#
# H1 (heterogeneity): higher within-GMA price dispersion predicts higher
# outlier concentration, holding price level constant. Sign expected
# positive on p90p10.

message("\nMain regression (with zone fixed effects)")

main_fit <- lm(
  pct_excl ~ p90p10 + log_median_sp + factor(zone),
  data = gma_panel
)

main_se <- coeftest(main_fit, vcov = vcovHC(main_fit, type = "HC3"))
print(main_se)


# ----------------------------------------------------------------------------
# Falsification: pure price level
# ----------------------------------------------------------------------------
#
# H0 (poverty alternative): if outlier concentration were just about poor
# neighborhoods, log(median_sp) alone should predict pct_excl strongly and
# negatively. If the heterogeneity story is right, log(median_sp) should
# have weaker explanatory content on its own than p90p10 has in the joint
# specification.

message("\nFalsification regression (price level only)")

falsify_fit <- lm(
  pct_excl ~ log_median_sp + factor(zone),
  data = gma_panel
)

falsify_se <- coeftest(falsify_fit, vcov = vcovHC(falsify_fit, type = "HC3"))
print(falsify_se)


# ----------------------------------------------------------------------------
# Save coefficient tables
# ----------------------------------------------------------------------------

tidy_coef <- function(ct, model_label) {
  as.data.frame(unclass(ct)) %>%
    rownames_to_column("term") %>%
    rename(estimate  = Estimate,
           std_error = `Std. Error`,
           t_value   = `t value`,
           p_value   = `Pr(>|t|)`) %>%
    mutate(model = model_label, .before = 1)
}

regression_results <- bind_rows(
  tidy_coef(main_se,    "main"),
  tidy_coef(falsify_se, "falsification_price_only")
)

write_csv(regression_results,
          here::here("data", "processed", "gma_regression_results.csv"))


# ----------------------------------------------------------------------------
# Figure 6: heterogeneity scatter (headline)
# ----------------------------------------------------------------------------
#
# Plot pct_excl on within-GMA p90/p10 with a linear fit. Each point is one
# GMA. This is the visual equivalent of the main coefficient.

beta_p90p10 <- coef(main_fit)["p90p10"]
r_p90p10    <- cor(gma_panel$p90p10, gma_panel$pct_excl)

fig6 <- ggplot(gma_panel, aes(p90p10, pct_excl * 100)) +
  geom_point(alpha = 0.4, color = PAL$blue, size = 1.4) +
  geom_smooth(method = "lm", se = TRUE,
              color = PAL$black, fill = "grey85", linewidth = 0.6) +
  labs(
    title    = "Within-GMA price heterogeneity predicts outlier concentration",
    subtitle = sprintf(
      "Each point is one Geographic Market Area, 2014-2017 (n = %d GMAs); r = %.2f",
      nrow(gma_panel), r_p90p10
    ),
    x       = "p90 / p10 of sale prices within GMA",
    y       = "Share of transactions with extreme ratio (%)",
    caption = paste0(
      "Source: Author's analysis. Outcome is share of transactions with ",
      "av/sale_price outside [0.5, 2.0].\n",
      "Linear fit overlay; main regression includes log(median sp) and zone FE."
    )
  ) +
  theme_project()

save_figure(fig6, "fig_06_gma_heterogeneity_scatter.pdf",
            orientation = "landscape")

message("\n  Wrote figures/fig_06_gma_heterogeneity_scatter.pdf")


# ----------------------------------------------------------------------------
# Figure 7: price falsification scatter
# ----------------------------------------------------------------------------
#
# Plot pct_excl on log(median sp) with a linear fit. The poverty story
# predicts a steep negative slope. The heterogeneity story predicts a
# weaker relationship that softens further once p90p10 enters the model.

r_logsp <- cor(gma_panel$log_median_sp, gma_panel$pct_excl)

fig7 <- ggplot(gma_panel, aes(log_median_sp, pct_excl * 100)) +
  geom_point(alpha = 0.4, color = PAL$gray, size = 1.4) +
  geom_smooth(method = "lm", se = TRUE,
              color = PAL$black, fill = "grey85", linewidth = 0.6) +
  labs(
    title    = "Price level alone is a weak predictor of outlier concentration",
    subtitle = sprintf(
      "Each point is one GMA, 2014-2017; r = %.2f", r_logsp
    ),
    x       = "log(median sale price) within GMA",
    y       = "Share of transactions with extreme ratio (%)",
    caption = paste0(
      "Source: Author's analysis. If outlier concentration were primarily ",
      "about low-priced neighborhoods,\n",
      "this plot would show a strong negative slope. Compare to fig_06."
    )
  ) +
  theme_project()

save_figure(fig7, "fig_07_gma_price_falsification.pdf",
            orientation = "landscape")

message("  Wrote figures/fig_07_gma_price_falsification.pdf")


# ----------------------------------------------------------------------------
# Summary numbers
# ----------------------------------------------------------------------------

message("\nKey numbers:")
message("  GMAs in sample: ", nrow(gma_panel))
message("  Main beta on p90p10: ",
        sprintf("%.3f", coef(main_fit)["p90p10"]),
        "  (HC3 SE: ",
        sprintf("%.3f", main_se["p90p10", "Std. Error"]), ")")
message("  Main beta on log_median_sp: ",
        sprintf("%.3f", coef(main_fit)["log_median_sp"]),
        "  (HC3 SE: ",
        sprintf("%.3f", main_se["log_median_sp", "Std. Error"]), ")")
message("  Falsification beta on log_median_sp (price-only): ",
        sprintf("%.3f", coef(falsify_fit)["log_median_sp"]),
        "  (HC3 SE: ",
        sprintf("%.3f", falsify_se["log_median_sp", "Std. Error"]), ")")
message("  Correlation p90p10 ~ pct_excl: ", sprintf("%.3f", r_p90p10))
message("  Correlation log_median_sp ~ pct_excl: ", sprintf("%.3f", r_logsp))

message("\nDone.")
