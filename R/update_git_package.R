#' Update Packages from GitHub
#'
#' A thin wrapper for \code{\link[pak]{pak}}. Installs or updates one or more
#' packages from GitHub repositories. If a list or vector of repositories is
#' provided, each is installed in order.
#'
#' @param repo A character string or character vector of repository addresses
#'   in the form \code{"username/repo"} (e.g., \code{"johnsonpc2/pcjtools"}).
#'
#' @returns Invisibly returns \code{NULL}. Called for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' # Install a single package
#' update_git_package("johnsonpc2/pcjtools")
#'
#' # Install multiple packages in order
#' update_git_package(c("johnsonpc2/pcjtools", "tidyverse/dplyr"))
#' }
update_git_package <- function(repo = NULL) {

  if (!interactive() || nchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) > 0L) {
    stop("update_git_package() can only be used in an interactive R session.")
  }
  stopifnot(is.character(repo), length(repo) >= 1L, all(nchar(repo) > 0L))

  for (r in repo) {
    pak::pak(r)
  }
  invisible(NULL)

}