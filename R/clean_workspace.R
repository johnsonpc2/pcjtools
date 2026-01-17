#' Tidy the RStudio Work Space
#'
#' Clear the console, plot window, and remove objects from the global
#'  environment.
#'
#' @param confirm Logical (defaults to TRUE). Should the function ask for
#'  confirmation before clearing the work space?
#' @returns Confirmation the environment has been cleaned.
#' @seealso [graphics.off()] and [rm()], for which clean_workspace serves
#'  as a wrapper.
#' @export
#' @examples
#' \donttest{
#' # Execute without asking (use with caution)
#' clean_workspace(confirm = FALSE)
#' }
clean_workspace <- function(confirm = TRUE) {

# Validation and Error Handling -------------------------------------------

  if (confirm && interactive()) {
    cat("1: Yes\n")
    cat("2: No\n")
    response <- readline(prompt = "Clear all objects from workspace?\n")

    if (response != "1") {
      message("Operation cancelled.")
      return(invisible(NULL))
    }
  }

# Cleaning Operations -----------------------------------------------------

  # Clear console
  cat("\014")

  # Clear graphs
  grDevices::graphics.off()

  # Remove objects from the global environment
  rm(list = ls(envir = .GlobalEnv, all.names = TRUE), envir = .GlobalEnv)


# Output ------------------------------------------------------------------

  message("Workspace cleaned")
  invisible(NULL)
}
