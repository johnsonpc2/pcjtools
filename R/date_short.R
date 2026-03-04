#' Date Strings
#'
#' A function to output simple formatted date strings
#'
#' @param time Logical. Should time be included in the short date string?
#'  Defaults to FALSE.
#'
#' @returns A string corresponding to the current system date
#' @export
#'
#' @examples
#' # Output the current date only
#' date_short()
#'
#' # Output the current date and time
#' date_short(time = TRUE)
date_short <- function(time = FALSE) {

  if (time == TRUE) {

    format(Sys.time(), "%Y%m%d%H%M")
  } else {

    format(Sys.time(), "%Y%m%d")

  }

}
