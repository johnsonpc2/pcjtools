#' Update Packages from Github
#'
#' A thin wrapper for install_github() from the devtools package. Used to update
#'  packages from Github repositories.
#'
#' @param repo A repository address in the form 'username/repo'
#'  (e.g., 'johnsonpc2/pcjtools').
#' @param upgrade Should the package's dependencies be upgraded? One of
#'  "default", "ask", "always", or "never"
#' @param force Force installation, even if the remote state has not changed
#'  since the previous install.
#'
#' @returns Invisibly updates packages from Github.
#' @export
#'
#' @examples
#' \dontrun{
#' update_git_package(repo = 'johnsonpc2/pcjtools')
#' }
update_git_package <- function(repo = NULL, upgrade = "always", force = FALSE) {

  # Hard stop if not in an interactive RStudio session
  if (!interactive() || nchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) > 0L) {
    stop("update_git_package() can only be used in an interactive R session.")
  }

  devtools::install_github(
    repo = repo,
    upgrade = upgrade,
    force = force
  )

}