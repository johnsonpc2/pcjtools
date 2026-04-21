#' Format a p-value string following APA 7 conventions
#'
#' Reports "< .001" for very small values, otherwise "= .XXX" to three
#' decimal places. Leading zero is omitted per APA 7 §6.38. Fully
#' vectorised, so it works correctly on scalars or vectors (e.g. inside
#' [mapply()]).
#'
#' @param p Numeric vector of p-values in \[0, 1\].
#' @returns A character vector of formatted p-value strings.
#' @export
format_p <- function(p) {
  rounded <- sprintf("%.3f", p)
  ifelse(p < .001,                "< .001",
         ifelse(rounded == "1.000",      "= .999",
                ifelse(rounded == "0.000",      "< .001",
                       paste0("= ", rounded))))
}