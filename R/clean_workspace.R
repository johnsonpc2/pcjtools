#' Work Space Cleaner
#'
#' Clear the console, plot window, and remove objects from the global
#'  environment.
#'
#' @param confirm Logical (default is TRUE). Should the function ask for
#'  confirmation before clearing?
#' @returns Confirmation that the environment has been cleaned.
#' @seealso [graphics.off()] and [rm()], which clean_workspace serves
#'  as a wrapper for.
#' @export
#' @examples
#' \donttest{
#' # Execute without asking (use with caution)
#' clean_workspace(confirm = FALSE)
#' }
clean_workspace <- function(confirm = TRUE) {

  if (confirm && interactive()) {
    cat("1: Yes\n")
    cat("2: No\n")
    response <- readline(prompt = "Clear all objects from workspace?\n")

    if (response != "1") {
      message("Operation cancelled.")
      return(invisible(NULL))
    }
  }

  # Clear console
  cat("\014")

  # Clear graphs
  grDevices::graphics.off()

  # Remove objects from the global environment
  rm(list = ls(envir = .GlobalEnv, all.names = TRUE), envir = .GlobalEnv)

  message("Workspace cleaned")
  invisible(NULL)
}
