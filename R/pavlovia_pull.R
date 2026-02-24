#' Pull Files from One or More Git Remotes
#'
#' Pulls files from one or more Git remote repositories directly from R
#' using the RStudio terminal. Remotes are pulled in the order specified,
#' allowing you to ensure the most recent backed-up work is retrieved before
#' pulling new data files. A pause is inserted between each pull to ensure
#' each command completes before the next begins. Only works in interactive
#' RStudio sessions and will never execute during R CMD check or automated
#' testing.
#'
#' @param remotes A named or unnamed list of character vectors, where each
#'   element specifies a remote and branch to pull from. Each element should
#'   be a character vector of length 2: c(remote, branch). Defaults to
#'   pulling from GitHub (origin main) first, then GitLab (gitlab master).
#' @param sleep A positive number. The number of seconds to wait between
#'   pulls to allow each command to complete before the next is sent.
#'   Defaults to 5.
#'
#' @returns Invisibly returns NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' # This function only works in an interactive RStudio session and will
#' # never be run during R CMD check.
#'
#' # Pull using defaults: GitHub first, then GitLab, 5 second pause between
#' pull()
#'
#' # Pull from GitHub only
#' pull(remotes = list(c("origin", "main")))
#'
#' # Pull from GitLab only
#' pull(remotes = list(c("gitlab", "master")))
#'
#' # Pull from GitHub first, then GitLab with a longer pause
#' pull(remotes = list(c("origin", "main"), c("gitlab", "master")),
#'      sleep = 30)
#'
#' # Pull from a custom remote and branch
#' pull(remotes = list(c("upstream", "dev")))
#' }
pull <- function(remotes = list(c("origin", "main"),
                                c("gitlab", "master")),
                 sleep = 5) {
  # Validate remotes argument
  stopifnot(is.list(remotes), length(remotes) >= 1L)
  for (r in remotes) {
    stopifnot(
      is.character(r),
      length(r) == 2L,
      all(nchar(r) > 0L)
    )
  }

  # Validate sleep argument
  stopifnot(
    is.numeric(sleep),
    length(sleep) == 1L,
    sleep >= 0
  )

  # Hard stop if not in an interactive RStudio session
  if (!interactive()) {
    stop("pull() can only be used in an interactive R session.")
  }
  if (!rstudioapi::isAvailable()) {
    stop("pull() requires RStudio to be running.")
  }

  # Get or create a terminal
  terminal_id <- rstudioapi::terminalVisible()
  if (is.null(terminal_id)) {
    terminal_id <- rstudioapi::terminalCreate()
  }

  # Pull from each remote in order, pausing between each
  for (i in seq_along(remotes)) {
    remote <- remotes[[i]][1]
    branch <- remotes[[i]][2]
    rstudioapi::terminalSend(
      terminal_id,
      sprintf("git pull %s %s --no-edit\n", remote, branch)
    )
    message(sprintf("Pulling from %s %s...", remote, branch))
    if (i < length(remotes)) {
      message(sprintf("Waiting %s seconds before next pull...", sleep))
      Sys.sleep(sleep)
    }
  }

  invisible(NULL)
}