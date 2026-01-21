#' Load a list of packages
#'
#' @param x A character list of packages to load
#'
#' @returns A message confirming the specified packages have been loaded, or an error.
#' @export
#'
#' @examples
#' \donttest{
#' packages <- c("data.table", "ggplot2")
#'
#' load_packages(x = packages)
#' }

load_packages <- function(x) {

  # Convert to character vector if needed
  if (is.list(x)) {
    packages <- unlist(x)
  } else {
    packages <- as.character(x)
  }

  # Load all packages
  loaded <- purrr::map_lgl(
    .x = packages,
    .f = ~ {
      suppressPackageStartupMessages(
        library(.x, character.only = TRUE, quietly = TRUE)
      )
      return(TRUE)
    }
  )

  # Report results
  if (all(loaded)) {
    message("Successfully loaded ", length(packages), " package(s)")
  } else {
    warning("Some packages failed to load")
  }

  invisible(loaded)
}