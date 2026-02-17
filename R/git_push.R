#' Send Git Commands to RStudio Terminal
#'
#' Stages, commits, and optionally pushes changes to a git repository
#' directly from R using the RStudio terminal.
#'
#' @param message A string. The commit message. Defaults to a timestamp backup
#'  message (e.g., "20260217 14:35 Backup).
#' @param add Logical. If TRUE (default), stages all changes with
#'   "git add ." before committing.
#' @param push Logical. If TRUE, pushes changes to the remote repository
#'   after committing. Defaults to FALSE.
#' @param remote A string. The remote and branch to push to, in the format
#'   "remote branch" (e.g., "origin main"). Defaults to "origin main".
#'   Only used when push is TRUE.
#'
#' @returns Invisibly returns NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' # This function only works in an interactive RStudio session and will
#' # never be run during R CMD check.
#' git_commit(message = "Updated analysis script")
#' git_commit(message = "Updated analysis script", push = TRUE)
#' git_commit(message = "Updated analysis script", push = TRUE,
#'            remote = "origin dev")
#' }
git_push <- function(message = paste(format(Sys.time(), "%Y%m%d %H:%M"), "Backup"),
                       add = TRUE,
                       push = FALSE,
                       remote = "origin main") {

  # Input validation
  if (!inherits(message, "character") || length(message) != 1L || nchar(message) == 0L) {
    stop("'message' must be a non-empty character string.")
  }
  if (!inherits(add, "logical") || length(add) != 1L) {
    stop("'add' must be a single logical value.")
  }
  if (!inherits(push, "logical") || length(push) != 1L) {
    stop("'push' must be a single logical value.")
  }
  if (!inherits(remote, "character") || length(remote) != 1L || nchar(remote) == 0L) {
    stop("'remote' must be a non-empty character string.")
  }

  # Hard stop if not in an interactive RStudio session
  if (!interactive()) {
    stop("git_commit() can only be used in an interactive R session.")
  }

  if (!rstudioapi::isAvailable()) {
    stop("git_commit() requires RStudio to be running.")
  }

  # Get or create a terminal
  terminal_id <- rstudioapi::terminalVisible()
  if (is.null(terminal_id)) {
    terminal_id <- rstudioapi::terminalCreate()
  }

  # Stage all changes
  if (isTRUE(add)) {
    rstudioapi::terminalSend(terminal_id, "git add .\n")
    message("Staging all changes...")
  }

  # Commit
  rstudioapi::terminalSend(
    terminal_id,
    sprintf('git commit -m "%s"\n', message)
  )
  message(sprintf("Committing with message: '%s'", message))

  # Push
  if (isTRUE(push)) {
    rstudioapi::terminalSend(
      terminal_id,
      sprintf("git push %s\n", remote)
    )
    message(sprintf("Pushing to %s...", remote))
  }

  invisible(NULL)
}