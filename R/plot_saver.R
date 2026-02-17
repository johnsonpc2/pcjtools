#' Efficiently Save Plot(s) to File with Consistent Settings
#'
#' @param plots A ggplot object or list of ggplot objects to save to file
#' @param dir A string or character vector containing the path to the directory
#'   where plot(s) will be saved.
#' @param names A name string, list of name strings that matches the length of
#'   the `plots` list, or `NULL` (the default). Names to give to the plot
#'   files. If a single name string and > 1 plot is provided, each plot will
#'   be given that name followed by a numeric counter.
#' @param use_device_size Logical. If TRUE (default), uses the current graphics
#'   device dimensions (e.g., the zoom window size). If FALSE, uses specified
#'   width and height.
#' @param preview Logical. If TRUE (default), opens each plot in a new window
#'   before saving so you can verify the layout. Only works in interactive
#'   sessions. If FALSE, saves directly without preview.
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
#' plot_saver(plots = g1, dir = tempdir(), preview = FALSE)
#' }
plot_saver <- function(plots,
                       dir = ".",
                       names = NULL,
                       use_device_size = TRUE,
                       preview = TRUE,
                       device = NULL,
                       path = NULL,
                       scale = 1,
                       width = NA,
                       height = NA,
                       units = "in",
                       dpi = 900,
                       limitsize = FALSE,
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

  if (!"units" %in% names(dots)) {
    dots$units <- units
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

  # Validate width and height - must be NA or a single numeric value
  stopifnot(
    (ggplot2::is_ggplot(plots) || is.list(plots)),
    (dir.exists(dir) && is.character(dir) && length(dir) == 1L),
    (is.null(names) ||
       ((length(names) == 1L || length(names) == length(plots)) &&
          is.character(names))),
    (is.na(width)  || (is.numeric(width)  && length(width)  == 1L)),
    (is.na(height) || (is.numeric(height) && length(height) == 1L))
  )

  # Resolve width and height once before the loop since they are the
  # same for all plots
  user_width  <- if (!is.na(width))  width  else NULL
  user_height <- if (!is.na(height)) height else NULL

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

    # Open preview window if requested AND in interactive session
    if (preview && interactive()) {
      if (.Platform$OS.type == "windows") {
        grDevices::windows()
      } else if (Sys.info()["sysname"] == "Darwin") {
        grDevices::quartz()
      } else {
        grDevices::x11()
      }

      print(plots[[i]])

      if (length(plots) > 1) {
        readline(prompt = sprintf(
          "Plot %d of %d displayed. Resize window if needed, then press [Enter] to save and continue: ",
          i, length(plots)
        ))
      } else {
        readline(prompt = "Resize window if needed, then press [Enter] to save: ")
      }
    }

    # Resolve dimensions for this plot
    iter_dots <- dots

    if (!is.null(user_width)) {
      iter_dots$width <- user_width
    } else if (use_device_size && preview && interactive() &&
               length(grDevices::dev.list()) > 0) {
      iter_dots$width <- grDevices::dev.size("in")[1]
    } else {
      iter_dots$width <- 10
    }

    if (!is.null(user_height)) {
      iter_dots$height <- user_height
    } else if (use_device_size && preview && interactive() &&
               length(grDevices::dev.list()) > 0) {
      iter_dots$height <- grDevices::dev.size("in")[2]
    } else {
      iter_dots$height <- 8
    }

    if (interactive()) {
      message(sprintf("Saving plot %d of %d at %g x %g inches: %s",
                      i, length(plots),
                      iter_dots$width, iter_dots$height,
                      filename[i]))
    }

    # Save the plot
    do.call(ggplot2::ggsave, c(
      list(
        filename = filename[i],
        plot = plots[[i]]
      ),
      iter_dots
    ))

    # Close the preview device if it was opened
    if (preview && interactive() && length(grDevices::dev.list()) > 0) {
      grDevices::dev.off()
    }
  }

  invisible(plots)
}