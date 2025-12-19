#' Clear the console and plot window, and remove objects from the environment
#'
#' @returns confirmation that the environment has been cleaned
#' @export
#'
#' @examples
#' \donttest{
#' clean_workspace()
#' }

clean_workspace <- function() {
  # Clear console
  cat("\014")

  # Clear graphs
  grDevices::graphics.off()

  # Remove objects from the environment
  rm(list = ls(all.names = TRUE))

  cat("Your workspace has been cleaned")
}
