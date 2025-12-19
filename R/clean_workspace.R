#' Clear the console and plot window, and remove objects from the environment
#'
#' @param ask Logical; should the function ask for confirmation before clearing?
#'   Defaults to TRUE for safety.
#'
#' @returns confirmation that the environment has been cleaned (invisibly)
#' @export
#'
#' @examples
#' \donttest{
#' # Clean workspace without asking (use with caution!)
#' clean_workspace(ask = FALSE)
#' }

clean_workspace <- function(ask = TRUE) {

  if (ask && interactive()) {
    response <- readline(prompt = "Clear all objects from workspace? (y/n): ")
    if (tolower(response) != "y") {
      message("Workspace cleaning cancelled.")
      return(invisible(NULL))
    }
  }

  # Clear console
  cat("\014")

  # Clear graphs
  grDevices::graphics.off()

  # Remove objects from the global environment
  rm(list = ls(envir = .GlobalEnv, all.names = TRUE), envir = .GlobalEnv)

  message("Your workspace has been cleaned")
  invisible(NULL)
}
