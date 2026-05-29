# ----------------------------------------------------------------------------
# R/iaao_metrics.R
#
# Reusable ratio-study statistics following the IAAO Standard on Ratio Studies.
#
# The four core measures:
#   - cod  : Coefficient of Dispersion. Uniformity around the median ratio.
#            IAAO standard for single-family residential: COD <= 15.
#   - prd  : Price-Related Differential. Detects vertical inequity in level.
#            IAAO standard: 0.98 <= PRD <= 1.03.
#   - prb  : Price-Related Bias. Detects vertical inequity in slope.
#            IAAO standard: -0.05 <= PRB <= 0.05.
#   - p9010 / p_iqr : percentile-based dispersion measures used in the
#            GMA-level analysis as alternatives to COD when sample sizes
#            within geographic units are small.
#
# All functions take a numeric vector of ratios (and, for PRD/PRB, a vector
# of sale prices) and return a single scalar. They are designed to be used
# inside dplyr summarise() calls.
#
# Missing values: na.rm = TRUE by default. If you want to detect missing-data
# problems, check upstream before calling these.
# ----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(stats)
})


#' Coefficient of Dispersion (COD).
#'
#' The COD measures uniformity around the median assessment ratio. It is the
#' average absolute deviation from the median, expressed as a percentage of
#' the median.
#'
#' Definition:
#'   COD = 100 * mean( |ratio - median(ratio)| ) / median(ratio)
#'
#' IAAO standard for single-family residential: COD <= 15.
#' Values above 15 indicate non-uniform assessment within the relevant
#' geographic unit.
#'
#' @param ratio Numeric vector of assessment ratios (av / sale_price).
#' @param na.rm Logical; if TRUE, NAs are removed before computation.
#' @return A single numeric value. NA if no non-missing observations.
cod_fn <- function(ratio, na.rm = TRUE) {
  if (na.rm) ratio <- ratio[!is.na(ratio) & is.finite(ratio)]
  if (length(ratio) == 0) return(NA_real_)
  
  med <- median(ratio)
  if (med == 0 || !is.finite(med)) return(NA_real_)
  
  100 * mean(abs(ratio - med)) / med
}


#' Price-Related Differential (PRD).
#'
#' The PRD detects vertical inequity in the *level* of assessments. It is
#' the ratio of the mean assessment ratio to the sales-price-weighted mean
#' ratio. A PRD above 1.03 indicates regressivity (lower-priced properties
#' are assessed at higher ratios than higher-priced ones). A PRD below 0.98
#' indicates progressivity.
#'
#' Definition:
#'   PRD = mean(ratio) / weighted.mean(ratio, sale_price)
#'
#' IAAO standard: 0.98 <= PRD <= 1.03.
#'
#' @param ratio Numeric vector of assessment ratios.
#' @param sale_price Numeric vector of sale prices, same length as ratio.
#' @param na.rm Logical; if TRUE, pairs with NA in either input are dropped.
#' @return A single numeric value. NA if no valid pairs.
prd_fn <- function(ratio, sale_price, na.rm = TRUE) {
  if (length(ratio) != length(sale_price)) {
    stop("prd_fn: ratio and sale_price must be the same length.")
  }
  
  if (na.rm) {
    ok <- !is.na(ratio) & !is.na(sale_price) &
      is.finite(ratio) & is.finite(sale_price) &
      sale_price > 0
    ratio <- ratio[ok]
    sale_price <- sale_price[ok]
  }
  if (length(ratio) == 0) return(NA_real_)
  
  num <- mean(ratio)
  den <- weighted.mean(ratio, w = sale_price)
  if (den == 0 || !is.finite(den)) return(NA_real_)
  
  num / den
}


#' Price-Related Bias (PRB).
#'
#' The PRB is the IAAO's preferred regression-based test for vertical inequity.
#' It is the slope coefficient from a regression of percentage deviation from
#' the median ratio on log value, where "value" is constructed as a 50/50
#' blend of the median-normalized assessed value and the sale price.
#'
#' Definition (IAAO 2013):
#'   Let med = median(ratio)
#'   Let dev = (ratio - med) / med
#'   Let value = 0.5 * (av / med) + 0.5 * sale_price
#'              = 0.5 * (ratio * sale_price / med) + 0.5 * sale_price
#'   PRB = slope from lm(dev ~ log(value) / log(2))
#'   Interpretation: a 1 percent change in value associates with a PRB
#'   change in ratio (expressed as fraction of median).
#'
#' IAAO standard: -0.05 <= PRB <= 0.05. Values outside this range indicate
#' meaningful vertical inequity.
#'
#' Note: PRB requires sufficient sample size to fit a regression. The function
#' returns NA if fewer than 10 valid observations are available.
#'
#' @param ratio Numeric vector of assessment ratios.
#' @param sale_price Numeric vector of sale prices, same length as ratio.
#' @param na.rm Logical; if TRUE, NA pairs are dropped.
#' @return A single numeric slope estimate. NA if regression cannot be fit.
prb_fn <- function(ratio, sale_price, na.rm = TRUE) {
  if (length(ratio) != length(sale_price)) {
    stop("prb_fn: ratio and sale_price must be the same length.")
  }
  
  if (na.rm) {
    ok <- !is.na(ratio) & !is.na(sale_price) &
      is.finite(ratio) & is.finite(sale_price) &
      sale_price > 0 & ratio > 0
    ratio <- ratio[ok]
    sale_price <- sale_price[ok]
  }
  if (length(ratio) < 10) return(NA_real_)
  
  med <- median(ratio)
  if (med == 0 || !is.finite(med)) return(NA_real_)
  
  dev   <- (ratio - med) / med
  value <- 0.5 * (ratio * sale_price / med) + 0.5 * sale_price
  if (any(value <= 0)) return(NA_real_)
  
  fit <- tryCatch(
    lm(dev ~ I(log(value) / log(2))),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NA_real_)
  
  unname(coef(fit)[2])
}


#' 90/10 percentile ratio of a numeric vector.
#'
#' Used as a dispersion measure in the GMA-level analysis, where small
#' within-unit sample sizes can make COD unstable. The 90/10 ratio is the
#' 90th percentile divided by the 10th percentile.
#'
#' @param x Numeric vector.
#' @param na.rm Logical.
#' @return p90 / p10. NA if either percentile is non-finite or p10 is zero.
p9010 <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x) & is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  
  qs <- quantile(x, c(0.10, 0.90), names = FALSE)
  if (qs[1] == 0 || !is.finite(qs[1]) || !is.finite(qs[2])) return(NA_real_)
  
  qs[2] / qs[1]
}


#' Interquartile range as a fraction of the median.
#'
#' Used as a robust scale-invariant dispersion measure alongside the 90/10
#' ratio. Less sensitive to extreme tails than p9010 but more sensitive than
#' the median absolute deviation.
#'
#' @param x Numeric vector.
#' @param na.rm Logical.
#' @return (Q3 - Q1) / median. NA if median is zero or non-finite.
p_iqr_fn <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x) & is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  
  med <- median(x)
  if (med == 0 || !is.finite(med)) return(NA_real_)
  
  qs <- quantile(x, c(0.25, 0.75), names = FALSE)
  (qs[2] - qs[1]) / med
}


#' Convenience wrapper: compute the standard IAAO panel for a data frame.
#'
#' Given a data frame with ratio and sale_price columns, return a one-row
#' tibble with the four core IAAO metrics plus sample size and median ratio.
#' Useful for summarise() over geographic or temporal groups.
#'
#' @param df A data frame containing columns named `ratio` and `sale_price`.
#' @return A one-row tibble with columns n, median_ratio, cod, prd, prb.
iaao_summary <- function(df) {
  tibble::tibble(
    n            = sum(!is.na(df$ratio) & is.finite(df$ratio)),
    median_ratio = median(df$ratio, na.rm = TRUE),
    cod          = cod_fn(df$ratio),
    prd          = prd_fn(df$ratio, df$sale_price),
    prb          = prb_fn(df$ratio, df$sale_price)
  )
}