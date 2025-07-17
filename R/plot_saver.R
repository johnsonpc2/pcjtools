#' Efficiently Save Plot(s) to File with Consistent Settings
#'
#' @param plots A ggplot object or list of gglot objects to save to file
#' @param dir A string or character vector containing the path to the directory
#'  where plot(s) will be saved.
#' @param names A name string, list of name strings that matches the length of
#'  the `'plot'` list, or `NULL` (the default). Names to give to the plot files.
#'  If a single name string and > 1 plot is provided, each plot will be
#'  given that name followed by a numeric counter.
#' @param ... Optional arguments to be passed to `'ggsave()'`.
#'
#' @returns (Invisibly) returns a list of the plots saved to file in the
#'  location given in `'dir'`.
#'
#' @export
#'
#' @examples
#' g1 <- ggplot2::ggplot(
#' data = mtcars,
#' mapping = ggplot2::aes(x = mpg, y = wt, color = factor(cyl))
#' )
#'
#' plot_saver(plots = g1, dir = tempdir())

plot_saver <- function(plots, dir = ".", names = NULL, ...) {

  stopifnot((is_ggplot(plots) || is.list(plots)),
            (dir.exists(dir) && is.character(dir) && length(dir) == 1L),
            (is.null(names) ||
               ((length(names) == 1L || length(names) == length(plots)) &&
                  is.character(names)))
  )

  if (is_ggplot(plots)) plots <- list(plots)

  date <- format(Sys.time(), "%Y%m%d_")

  filename <- NULL

  if (length(names) == 0L) {
    filename <- paste0(dir, "\\", date,  "Plot.png")

  } else if (length(names) == 1L) {
    for (i in seq_along(plots)) {
      filename[i] <- paste0(dir, "\\", date, names, i, ".png")
    }

  } else if (length(names) == length(plots)) {
    for (i in seq_along(plots)) {
      filename[i] <- paste0(dir, "\\", date, names[i], ".png")
    }

  } else {stop("Names must match length of plots, or have a length of 1.")}

  for (i in seq_along(plots)) {

    ggplot2::ggsave(
      filename = filename[i],
      plot = plots[[i]],
      dpi = 900,
      bg = "transparent"
    )

  }
}
