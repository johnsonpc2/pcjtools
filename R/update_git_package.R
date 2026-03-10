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
#' \donttest{
#' update_git_package(repo = 'johnsonpc2/pcjtools')
#' }
update_git_package <- function(repo = NULL, upgrade = "always", force = FALSE) {

  devtools::install_github(
    repo = repo,
    upgrade = upgrade,
    force = force
  )

}