#' Check Git Remote Reachability
#'
#' Tests whether a remote host is reachable over TCP on port 443 by attempting
#' a socket connection. Used internally to detect network issues before
#' executing Git operations, avoiding hung terminals or cryptic error messages
#' when no connection is available.
#'
#' A TCP handshake is preferred over a DNS lookup because DNS resolution can
#' succeed even on captive portal networks (e.g. hotel or airport Wi-Fi) where
#' the actual connection would fail or hang.
#'
#' @param host A single character string. The hostname to check. Defaults to
#'   \code{"github.com"}.
#' @param timeout A positive number. Seconds to wait before treating the
#'   connection attempt as a failure. Defaults to \code{5}.
#'
#' @returns A single logical. \code{TRUE} if the host was reachable on port 443
#'   within the timeout window; \code{FALSE} otherwise.
#'
#' @noRd
.git_reachable <- function(host = "github.com", timeout = 5) {

  if (!interactive() || nchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) > 0L) {
    return(FALSE)
  }

  conn <- tryCatch(
    socketConnection(host, port = 443, open = "rb", timeout = timeout),
    error = function(e) NULL,
    warning = function(w) NULL
  )

  if (is.null(conn)) return(FALSE)
  close(conn)
  TRUE
}