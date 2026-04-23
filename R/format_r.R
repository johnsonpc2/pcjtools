#' Format a correlation coefficient following APA 7 conventions
#'
#' Omits the leading zero (per APA 7 §6.38), reports to two decimal places,
#' and suppresses a negative sign when rounding produces "-0.00" so the
#' result reads "= .00" rather than "= -.00". Fully vectorised via ifelse().
#'
#' @param r Numeric vector of correlation coefficients in \[-1, 1\].
#' @returns A character vector of formatted strings, each beginning with "= ".
#' @export
format_r <- function(r) {
  # Round to 2 decimal places and strip leading zero
  formatted <- sub("0\\.", ".", sprintf("%.2f", abs(r)))
  # Restore sign, but suppress negative when rounded value is zero
  signed <- ifelse(r < 0 & formatted != ".00",
                   paste0("= -", formatted),
                   paste0("= ",   formatted))
  signed
}