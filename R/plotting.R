# ----------------------------------------------------------------------------
# R/plotting.R
#
# Shared ggplot2 theme, palette, and small plotting helpers used by every
# figure-producing script in analysis/.
#
# The goal is consistency: every figure in the repository should look like
# it belongs to the same project, without each script having to redefine
# its own theme.
#
# All figures are PDF outputs, designed to render cleanly at 9 x 5 inches
# for landscape figures and 7 x 7 inches for square figures.
# ----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})


#' Project palette.
#'
#' Five-color palette designed to be colorblind-friendly and to print
#' legibly in grayscale. The colors are:
#'   - blue    : the primary positive/neutral category
#'   - yellow  : the warning / borderline category
#'   - red     : the failing / negative category
#'   - gray    : axes, gridlines, neutral text
#'   - black   : titles and emphasized text
#'
#' Used throughout the IAAO-standard figures where COD <= 15 (blue),
#' 15 < COD <= 30 (yellow), COD > 30 (red).
PAL <- list(
  blue   = "#1696d2",
  yellow = "#fdbf11",
  red    = "#db2b27",
  gray   = "#6e6e6e",
  black  = "#000000"
)


#' Project ggplot2 theme.
#'
#' Built on theme_minimal with adjustments for:
#'   - Cleaner gridlines (horizontal only by default, no minor grid)
#'   - Larger plot titles relative to axis text
#'   - Caption styling that reads as a footnote, not a header
#'   - Sans-serif typography for screen and print legibility
#'
#' @param base_size Base font size. Default 11.
#' @return A ggplot2 theme object.
theme_project <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title        = element_text(face = "bold", size = rel(1.2),
                                       margin = margin(b = 6)),
      plot.subtitle     = element_text(size = rel(0.95), color = PAL$gray,
                                       margin = margin(b = 12)),
      plot.caption      = element_text(size = rel(0.8), color = PAL$gray,
                                       hjust = 0, margin = margin(t = 10)),
      axis.title        = element_text(size = rel(0.95)),
      axis.text         = element_text(color = PAL$gray),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "#e6e6e6", linewidth = 0.3),
      legend.title      = element_text(size = rel(0.9), face = "plain"),
      legend.text       = element_text(size = rel(0.85)),
      strip.text        = element_text(face = "bold", size = rel(0.95))
    )
}


#' Classify a COD value by the IAAO standard.
#'
#' Returns the project palette color corresponding to the IAAO uniformity
#' standard for single-family residential property. Used to color bars and
#' points in the descriptive analysis.
#'
#' @param cod Numeric vector of COD values.
#' @return Character vector of hex color codes.
cod_color <- function(cod) {
  dplyr::case_when(
    is.na(cod)  ~ PAL$gray,
    cod <= 15   ~ PAL$blue,
    cod <= 30   ~ PAL$yellow,
    TRUE        ~ PAL$red
  )
}


#' Save a ggplot to the figures directory at the project's standard sizes.
#'
#' Wrapper around ggsave that enforces project conventions: PDF output,
#' standard sizes by orientation, white background, and consistent dpi
#' for any rasterized layers.
#'
#' @param plot A ggplot object.
#' @param filename Filename relative to figures/, including extension.
#' @param orientation One of "landscape" (9x5), "square" (7x7), or "tall" (6x7).
#' @param fig_dir Directory to save into. Defaults to "figures".
#' @return Invisibly returns the full path written.
save_figure <- function(plot,
                        filename,
                        orientation = c("landscape", "square", "tall"),
                        fig_dir = "figures") {
  orientation <- match.arg(orientation)
  dims <- switch(orientation,
                 landscape = list(width = 9, height = 5),
                 square    = list(width = 7, height = 7),
                 tall      = list(width = 6, height = 7))
  
  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
  path <- file.path(fig_dir, filename)
  
  ggsave(path, plot,
         width  = dims$width,
         height = dims$height,
         units  = "in",
         dpi    = 300,
         bg     = "white")
  invisible(path)
}