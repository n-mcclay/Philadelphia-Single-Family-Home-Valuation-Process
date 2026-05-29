# ----------------------------------------------------------------------------
# analysis/03_flag_diagnostics.R
#
# Diagnostic analysis of data-quality flags constructed by R/flag_construction.R.
# This script implements the corrected version of the forensic work that
# motivated the redesign of this project.
#
# The substantive question:
#   With ratio-derived flags excluded from the predictor set, do administrative-
#   integrity flags still predict whether a transaction has an extreme assessment
#   ratio, conditional on price?
#
# The script produces three figures and two summary tables:
#   fig_03_flag_phi_matrix.pdf          -- pairwise phi correlations between
#                                          flag indicators.
#   fig_04_flag_fire_rates_by_av.pdf    -- heatmap of flag fire rates within
#                                          assessed-value bins.
#   fig_05_extreme_ratio_by_av.pdf      -- share of transactions with an extreme
#                                          assessment ratio by AV bin.
#   data/processed/flag_diagnostics.csv -- overall flag fire rates.
#   data/processed/flag_falsification.csv -- logit coefficients with tract-
#                                          clustered standard errors.
#
# Reads:
#   data/processed/master_philadelphia_2014_2025_with_gma.csv
#
# Note: this analysis uses the FULL post-arms-length sample, not the in-trim
# subset. The diagnostic question is about flagged-out transactions; trimming
# them would defeat the point.
# ----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(tibble)
  library(forcats)
  library(scales)
  library(sandwich)
  library(lmtest)
})

source(here::here("R", "flag_construction.R"))
source(here::here("R", "plotting.R"))


# ----------------------------------------------------------------------------
# Load and construct flags
# ----------------------------------------------------------------------------

master <- read_csv(
  here::here("data", "processed",
             "master_philadelphia_2014_2025_with_gma.csv"),
  show_col_types = FALSE
)

flagged <- construct_flags(master)

flag_vars <- flag_names(include_nominal = FALSE)

message("Sample size: ", format(nrow(flagged), big.mark = ","))
message("Flag set: ", paste(flag_vars, collapse = ", "))


# ----------------------------------------------------------------------------
# Overall flag fire rates
# ----------------------------------------------------------------------------

fire_rates <- tibble(
  flag      = flag_vars,
  fire_rate = sapply(flag_vars, function(f) mean(flagged[[f]], na.rm = TRUE)),
  n_fires   = sapply(flag_vars, function(f) sum(flagged[[f]], na.rm = TRUE))
) %>% arrange(desc(fire_rate))

print(fire_rates)

write_csv(fire_rates,
          here::here("data", "processed", "flag_diagnostics.csv"))


# ----------------------------------------------------------------------------
# Figure 3: pairwise phi correlation matrix between flag indicators
# ----------------------------------------------------------------------------

message("\nComputing flag co-occurrence matrix")

flag_mat <- flagged %>%
  select(all_of(flag_vars)) %>%
  mutate(across(everything(), as.integer)) %>%
  as.matrix()

# For binary variables, Pearson r equals the phi coefficient.
phi <- cor(flag_mat, use = "pairwise.complete.obs")

phi_long <- as.data.frame(phi) %>%
  rownames_to_column("x") %>%
  pivot_longer(-x, names_to = "y", values_to = "r") %>%
  mutate(
    x = factor(x, levels = flag_vars),
    y = factor(y, levels = rev(flag_vars))
  )

fig3 <- ggplot(phi_long, aes(x, y, fill = r)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.2f", r)),
            size = 2.8,
            color = ifelse(abs(phi_long$r) > 0.5, "white", PAL$black)) +
  scale_fill_gradient2(low = PAL$red, mid = "white", high = PAL$blue,
                       midpoint = 0, limits = c(-1, 1), name = "Phi") +
  scale_x_discrete(labels = function(x) gsub("_flag$", "", x)) +
  scale_y_discrete(labels = function(x) gsub("_flag$", "", x)) +
  labs(
    title    = "Pairwise flag co-occurrence (phi coefficient)",
    subtitle = "Linear association between administrative-integrity flag indicators",
    x = NULL, y = NULL,
    caption  = "Source: Author's analysis. Phi = Pearson correlation between binary flag indicators."
  ) +
  theme_project() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid  = element_blank())

save_figure(fig3, "fig_03_flag_phi_matrix.pdf", orientation = "square")

message("  Wrote figures/fig_03_flag_phi_matrix.pdf")


# ----------------------------------------------------------------------------
# Figure 4: flag fire rates by assessed-value bin
# ----------------------------------------------------------------------------

message("\nComputing flag fire rates by AV bin")

av_breaks <- c(0, 25000, 50000, 100000, 200000, 400000, 800000, Inf)
av_labels <- c("<$25K", "$25-50K", "$50-100K", "$100-200K",
               "$200-400K", "$400-800K", ">$800K")

by_av <- flagged %>%
  filter(!is.na(av), av > 0) %>%
  mutate(av_bin = cut(av, breaks = av_breaks, labels = av_labels,
                      include.lowest = TRUE)) %>%
  group_by(av_bin) %>%
  summarise(
    n_total = n(),
    across(all_of(flag_vars), \(x) mean(x, na.rm = TRUE),
           .names = "rate_{.col}"),
    .groups = "drop"
  )

by_av_long <- by_av %>%
  pivot_longer(starts_with("rate_"), names_to = "flag", values_to = "fire_rate") %>%
  mutate(flag = gsub("^rate_|_flag$", "", flag),
         flag = factor(flag, levels = gsub("_flag$", "", flag_vars)))

fig4 <- ggplot(by_av_long, aes(av_bin, fct_rev(flag), fill = fire_rate)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(fire_rate >= 0.005,
                               percent(fire_rate, accuracy = 0.1),
                               "")),
            size = 2.7,
            color = ifelse(by_av_long$fire_rate > 0.30, "white", PAL$black)) +
  scale_fill_gradient(low = "white", high = PAL$blue,
                      labels = percent_format(accuracy = 1),
                      name = "Fire rate") +
  labs(
    title    = "Flag fire rates by assessed-value range",
    subtitle = "Share of transactions with each flag firing, within AV bin",
    x        = "Assessed-value range",
    y        = NULL,
    caption  = "Source: Author's analysis. Empty cells indicate fire rate below 0.5%."
  ) +
  theme_project() +
  theme(panel.grid = element_blank())

save_figure(fig4, "fig_04_flag_fire_rates_by_av.pdf", orientation = "landscape")

message("  Wrote figures/fig_04_flag_fire_rates_by_av.pdf")


# ----------------------------------------------------------------------------
# Figure 5: share of transactions with an extreme ratio, by AV bin
# ----------------------------------------------------------------------------

message("\nComputing extreme-ratio concentration by AV bin")

extreme_by_av <- flagged %>%
  filter(!is.na(av), av > 0, !is.na(ratio), is.finite(ratio)) %>%
  mutate(
    av_bin       = cut(av, breaks = av_breaks, labels = av_labels,
                       include.lowest = TRUE),
    extreme      = ratio < 0.5 | ratio > 2.0,
    over_assess  = ratio > 2.0,
    under_assess = ratio < 0.5
  ) %>%
  group_by(av_bin) %>%
  summarise(
    n               = n(),
    extreme_rate    = mean(extreme),
    over_rate       = mean(over_assess),
    under_rate      = mean(under_assess),
    se              = sqrt(extreme_rate * (1 - extreme_rate) / n),
    .groups         = "drop"
  )

print(extreme_by_av)

fig5 <- ggplot(extreme_by_av, aes(av_bin, extreme_rate)) +
  geom_col(fill = PAL$blue, width = 0.7) +
  geom_errorbar(aes(ymin = extreme_rate - 1.96 * se,
                    ymax = extreme_rate + 1.96 * se),
                width = 0.2, color = PAL$black) +
  geom_text(aes(label = percent(extreme_rate, accuracy = 0.1)),
            vjust = -0.5, size = 3.2) +
  geom_text(aes(label = paste0("n=", comma(n)), y = 0.015),
            size = 2.6, color = "white") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Extreme assessment ratios concentrate in low-value properties",
    subtitle = "Share of transactions with av/sale_price outside [0.5, 2.0], by AV range",
    x        = "Assessed-value range",
    y        = "Share with extreme ratio",
    caption  = "Source: Author's analysis. Error bars: 95% confidence intervals."
  ) +
  theme_project()

save_figure(fig5, "fig_05_extreme_ratio_by_av.pdf", orientation = "landscape")

message("  Wrote figures/fig_05_extreme_ratio_by_av.pdf")


# ----------------------------------------------------------------------------
# The falsification test: do admin flags predict extreme ratios?
# ----------------------------------------------------------------------------
#
# Outcome: extreme = ratio < 0.5 | ratio > 2.0
# Predictors: the admin-integrity flag set + log(av) as price control
# Standard errors: clustered at the census tract level
#
# The key methodological point is that NONE of the predictors are derived
# from the ratio. If the coefficients are statistically and economically
# meaningful, that is evidence the original Paper 1 finding survives in
# modified form. If they collapse to zero, the original finding was an
# artifact of the circular flag.

message("\nFalsification logit")

logit_data <- flagged %>%
  filter(!is.na(av), av > 0, !is.na(ratio), is.finite(ratio),
         !is.na(census_tract)) %>%
  mutate(
    extreme = as.integer(ratio < 0.5 | ratio > 2.0),
    log_av  = log(av)
  )

# Note on the predictor set: fmv_identity_flag is omitted from this regression
# because, after the CLR-aware redefinition in R/flag_construction.R, only
# 2 transactions in the working sample fire the flag. That number is too
# small to estimate a coefficient reliably (the logit would otherwise produce
# a near-separation artifact). The flag is retained in the descriptive
# figures and the diagnostics CSV, where its near-zero fire rate is itself
# an informative finding: after accounting for the AVI-period CLR identity,
# genuine av/fmv field-copy errors are essentially absent in this sample.

falsification <- glm(
  extreme ~ bad_year_built_flag + bad_sqft_flag +
    room_logic_flag +
    round_price_flag + log_av,
  family = binomial(link = "logit"),
  data   = logit_data
)

clustered <- coeftest(
  falsification,
  vcov    = vcovCL,
  cluster = ~ census_tract
)

print(clustered)

# Save the coefficient table
falsification_table <- as.data.frame(unclass(clustered)) %>%
  rownames_to_column("term") %>%
  rename(estimate     = Estimate,
         std_error    = `Std. Error`,
         z_value      = `z value`,
         p_value      = `Pr(>|z|)`)

write_csv(falsification_table,
          here::here("data", "processed", "flag_falsification.csv"))


# ----------------------------------------------------------------------------
# Summary numbers
# ----------------------------------------------------------------------------

message("\nKey numbers:")
message("  Total transactions analyzed: ",
        format(nrow(flagged), big.mark = ","))
message("  Share with any flag firing: ",
        round(mean(flagged$n_flags > 0, na.rm = TRUE) * 100, 1), "%")
message("  Share with extreme ratio (overall): ",
        round(mean(logit_data$extreme) * 100, 1), "%")
message("  Share with extreme ratio (AV < $25K): ",
        round(extreme_by_av$extreme_rate[extreme_by_av$av_bin == "<$25K"] * 100, 1), "%")
message("  Share with extreme ratio (AV >= $400K): ",
        round(extreme_by_av$extreme_rate[extreme_by_av$av_bin == "$400-800K"] * 100, 1), "%")

message("\nDone.")