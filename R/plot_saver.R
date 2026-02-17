#' Efficiently Save Plot(s) to File with Consistent Settings
#'
#' @param plots A ggplot object or list of ggplot objects to save to file
#' @param dir A string or character vector containing the path to the directory
#'   where plot(s) will be saved.
#' @param names A name string, list of name strings that matches the length of
#'   the `plots` list, or `NULL` (the default). Names to give to the plot
#'   files. If a single name string and > 1 plot is provided, each plot will
#'   be given that name followed by a numeric counter.
#' @inheritParams ggplot2::ggsave
#'
#' @returns (Invisibly) returns a list of the plots saved to file in the
#'   location given in `dir`.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#'
#' g1 <- ggplot(
#'   data = mtcars,
#'   mapping = aes(x = mpg, y = wt, color = factor(cyl))
#' ) +
#'   geom_point()
#'
#' plot_saver(plots = g1, dir = tempdir())
#' }
plot_saver <- function(plots,
                       dir = ".",
                       names = NULL,
                       device = NULL,
                       path = NULL,
                       scale = 1,
                       width = NA,
                       height = NA,
                       units = c("in", "cm", "mm", "px"),
                       dpi = 900,
                       limitsize = TRUE,
                       bg = "transparent",
                       create.dir = FALSE,
                       ...) {

  # Capture dots for any additional arguments
  dots <- list(...)

  # Set defaults for ggsave arguments if not provided by user
  if (!"device" %in% names(dots)) {
    dots$device <- device
  }

  if (!"path" %in% names(dots)) {
    dots$path <- path
  }

  if (!"scale" %in% names(dots)) {
    dots$scale <- scale
  }

  if (!"width" %in% names(dots)) {
    dots$width <- width
  }

  if (!"height" %in% names(dots)) {
    dots$height <- height
  }

  if (!"units" %in% names(dots)) {
    dots$units <- match.arg(units)
  }

  if (!"dpi" %in% names(dots)) {
    dots$dpi <- dpi
  }

  if (!"limitsize" %in% names(dots)) {
    dots$limitsize <- limitsize
  }

  if (!"bg" %in% names(dots)) {
    dots$bg <- bg
  }

  if (!"create.dir" %in% names(dots)) {
    dots$create.dir <- create.dir
  }

  # Input validation
  stopifnot(
    (ggplot2::is_ggplot(plots) || is.list(plots)),
    (dir.exists(dir) && is.character(dir) && length(dir) == 1L),
    (is.null(names) ||
       ((length(names) == 1L || length(names) == length(plots)) &&
          is.character(names)))
  )

  # Convert single plot to list
  if (ggplot2::is_ggplot(plots)) plots <- list(plots)

  # Generate filenames
  date <- format(Sys.time(), "%Y%m%d_")
  filename <- NULL

  if (length(names) == 0L) {
    filename <- file.path(dir, paste0(date, "Plot.png"))
  } else if (length(names) == 1L) {
    filename <- vapply(seq_along(plots), function(i) {
      file.path(dir, paste0(date, names, i, ".png"))
    }, character(1))
  } else if (length(names) == length(plots)) {
    filename <- vapply(seq_along(plots), function(i) {
      file.path(dir, paste0(date, names[i], ".png"))
    }, character(1))
  } else {
    stop("Names must match length of plots, or have a length of 1.")
  }

  # Save plots
  for (i in seq_along(plots)) {
    do.call(ggplot2::ggsave, c(
      list(
        filename = filename[i],
        plot = plots[[i]]
      ),
      dots
    ))
  }

  invisible(plots)
}