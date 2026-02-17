#' Pull Files from Pavlovia GitLab Remote
#'
#' Pulls files from a Pavlovia GitLab remote repository directly from R
#' using the RStudio terminal. Only works in interactive RStudio sessions
#' and will never execute during R CMD check or automated testing.
#'
#' @param remote A string. The remote name to pull from. Defaults to
#'   "gitlab".
#' @param branch A string. The branch name to pull from. Defaults to
#'   "master".
#'
#' @returns Invisibly returns NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' # This function only works in an interactive RStudio session and will
#' # never be run during R CMD check.
#'
#' # Pull from default remote and branch (gitlab master)
#' pavlovia_pull()
#'
#' # Pull from a different branch
#' pavlovia_pull(branch = "dev")
#'
#' # Pull from a different remote and branch
#' pavlovia_pull(remote = "origin", branch = "main")
#' }
pavlovia_pull <- function(remote = "gitlab",
                          branch = "master") {

  stopifnot(
    is.character(remote) && length(remote) == 1L && nchar(remote) > 0L,
    is.character(branch) && length(branch) == 1L && nchar(branch) > 0L
  )

  # Hard stop if not in an interactive RStudio session
  if (!interactive()) {
    stop("pavlovia_pull() can only be used in an interactive R session.")
  }

  if (!rstudioapi::isAvailable()) {
    stop("pavlovia_pull() requires RStudio to be running.")
  }

  # Get or create a terminal
  terminal_id <- rstudioapi::terminalVisible()
  if (is.null(terminal_id)) {
    terminal_id <- rstudioapi::terminalCreate()
  }

  # Send pull command with --no-edit always appended
  rstudioapi::terminalSend(
    terminal_id,
    sprintf("git pull %s %s --no-edit\n", remote, branch)
  )
  message(sprintf("Pulling from %s %s...", remote, branch))

  invisible(NULL)
}