# ----------------------------------------------------------------------------
# analysis/02_iaao_trajectory.R
#
# Computes the citywide trajectory of IAAO ratio-study metrics (COD, PRD, PRB)
# from 2014 to 2025 and the zone-level coefficient of dispersion for the
# IAAO sales window of the tax year 2019 reassessment (2014-2017).
#
# Two figures are produced:
#   fig_01_citywide_trajectory.pdf -- three-panel year x metric trajectory,
#                                      with IAAO standard bands shaded.
#   fig_02_zone_cod.pdf            -- zone-level COD bar chart, colored by
#                                      IAAO compliance (blue/yellow/red).
#
# One summary table is written for downstream use:
#   data/processed/iaao_yearly.csv -- yearly n, median ratio, COD, PRD, PRB.
#
# Reads:
#   data/processed/master_philadelphia_2014_2025_with_gma.csv (produced
#     by analysis/01_build_master_panel.R)
#
# All ratio-study metrics are computed on in-trim records only, following
# IAAO standard practice.
# ----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(tibble)
})

source(here::here("R", "iaao_metrics.R"))
source(here::here("R", "plotting.R"))


# ----------------------------------------------------------------------------
# Load and scope to in-trim sample
# ----------------------------------------------------------------------------

master <- read_csv(
  here::here("data", "processed",
             "master_philadelphia_2014_2025_with_gma.csv"),
  show_col_types = FALSE
)

sample <- master %>%
  filter(in_trim, !is.na(ratio), !is.na(sale_price), sale_price > 0)

message("In-trim analytical sample: ",
        format(nrow(sample), big.mark = ","), " transactions")


# ----------------------------------------------------------------------------
# Citywide trajectory
# ----------------------------------------------------------------------------

message("\nComputing citywide trajectory (2014-2025)")

trajectory <- sample %>%
  group_by(year) %>%
  summarise(
    n            = n(),
    median_ratio = median(ratio, na.rm = TRUE),
    cod          = cod_fn(ratio),
    prd          = prd_fn(ratio, sale_price),
    prb          = prb_fn(ratio, sale_price),
    .groups      = "drop"
  )

print(trajectory)

write_csv(trajectory,
          here::here("data", "processed", "iaao_yearly.csv"))


# ----------------------------------------------------------------------------
# Figure 1: citywide trajectory, three panels
# ----------------------------------------------------------------------------

# Reshape to long form and label panels with descriptive names
traj_long <- trajectory %>%
  select(year, COD = cod, PRD = prd, PRB = prb) %>%
  pivot_longer(-year, names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, levels = c("COD", "PRD", "PRB")))

# IAAO standard bands per metric, as a data frame the facet can use
iaao_bands <- tibble(
  metric = factor(c("COD", "PRD", "PRB"),
                  levels = c("COD", "PRD", "PRB")),
  ymin   = c(0,    0.98, -0.05),
  ymax   = c(15,   1.03,  0.05)
)

fig1 <- ggplot(traj_long, aes(x = year, y = value)) +
  geom_rect(data = iaao_bands,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
            fill = "grey90", inherit.aes = FALSE) +
  geom_line(color = PAL$blue, linewidth = 0.8) +
  geom_point(color = PAL$blue, size = 2) +
  facet_wrap(~ metric, scales = "free_y", nrow = 1) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2)) +
  labs(
    title    = "Citywide IAAO ratio-study metrics, 2014-2025",
    subtitle = "Shaded bands indicate IAAO acceptable ranges for single-family residential",
    x        = NULL,
    y        = NULL,
    caption  = "Source: Author's analysis of Philadelphia RTT and OPA assessment data. In-trim sample."
  ) +
  theme_project()

save_figure(fig1, "fig_01_citywide_trajectory.pdf", orientation = "landscape")

message("  Wrote figures/fig_01_citywide_trajectory.pdf")


# ----------------------------------------------------------------------------
# Zone-level COD for the IAAO sales window of the tax year 2019 reassessment
# ----------------------------------------------------------------------------

message("\nComputing zone-level COD for 2014-2017 IAAO window")

zone_window <- sample %>%
  filter(year >= 2014, year <= 2017, !is.na(zone))

zone_cod <- zone_window %>%
  group_by(zone) %>%
  summarise(
    n            = n(),
    median_ratio = median(ratio, na.rm = TRUE),
    cod          = cod_fn(ratio),
    prd          = prd_fn(ratio, sale_price),
    prb          = prb_fn(ratio, sale_price),
    .groups      = "drop"
  ) %>%
  arrange(cod)

print(zone_cod)

message("  Zones meeting IAAO COD <= 15: ",
        sum(zone_cod$cod <= 15, na.rm = TRUE),
        " of ", nrow(zone_cod))


# ----------------------------------------------------------------------------
# Figure 2: zone-level COD bar chart
# ----------------------------------------------------------------------------

fig2 <- zone_cod %>%
  mutate(
    zone      = factor(zone, levels = zone),  # preserve sort order by COD
    fill_col  = cod_color(cod)
  ) %>%
  ggplot(aes(zone, cod, fill = fill_col)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 15, linetype = "dashed", color = PAL$black) +
  geom_text(aes(label = sprintf("%.1f", cod)),
            vjust = -0.4, size = 3.2, color = PAL$black) +
  scale_fill_identity() +
  annotate("text", x = 1.4, y = 17,
           label = "IAAO standard (COD <= 15)",
           hjust = 0, size = 3.2, color = PAL$gray) +
  labs(
    title    = "Zone-level coefficient of dispersion, 2014-2017",
    subtitle = "IAAO sales window for the tax year 2019 reassessment",
    x        = "GMA zone",
    y        = "Coefficient of dispersion (COD)",
    caption  = paste0("Source: Author's analysis. Zones colored by IAAO compliance: ",
                      "blue <= 15, yellow <= 30, red > 30.")
  ) +
  theme_project() +
  theme(panel.grid.major.y = element_line(color = "#e6e6e6", linewidth = 0.3))

save_figure(fig2, "fig_02_zone_cod.pdf", orientation = "landscape")

message("  Wrote figures/fig_02_zone_cod.pdf")


# ----------------------------------------------------------------------------
# Summary numbers for downstream use
# ----------------------------------------------------------------------------

message("\nKey numbers:")
message("  2014 COD: ", round(trajectory$cod[trajectory$year == 2014], 1))
message("  2025 COD: ", round(trajectory$cod[trajectory$year == 2025], 1))
message("  Most recent PRD: ",
        round(trajectory$prd[trajectory$year == max(trajectory$year)], 3))
message("  Most recent PRB: ",
        round(trajectory$prb[trajectory$year == max(trajectory$year)], 3))
message("  Zones failing IAAO COD <= 15 (2014-2017): ",
        sum(zone_cod$cod > 15, na.rm = TRUE), " of ", nrow(zone_cod))

message("\nDone.")
