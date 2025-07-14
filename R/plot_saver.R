#' Efficiently Save Plot(s) to File with Consistent Settings
#'
#' @param plots A ggplot object or list of gglot objects to save to file
#' @param dir
#' @param names A name string, list of name strings that matches the length of
#'  the `'plot'` list, or `NULL` (the default). Names to give to the plot files.
#'  If a single name string and > 1 plot is provided, each plot will be
#'  given that name followed by a numeric counter.
#' @param ... Optional arguments to be passed to `'ggsave()'`.
#'
#' @returns
#'
#' @export
#'
#' @examples
#' g1 <- ggplot2::ggplot(
#' data = mtcars,
#' mapping = ggplot2::aes(x = mpg, y = wt, color = factor(cyl))
#' )
#'
#' plot_saver(plots = g1)


plot_saver <- function(plots, dir = ".", names = NULL, ...) {

  # Input validation
  if (!is.character(dir)) {
    stop("'dir' must be a character string", call. = FALSE)
  }

  if (!is.null(names) && !is.character(names)) {
    stop("'names' must be either NULL or a character vector", call. = FALSE)
  }

  # Check if directory exists
  if (!dir.exists(dir)) {
    stop("Directory '", dir, "' does not exist", call. = FALSE)
  }

  # Case 1: Single ggplot object
  if (is.ggplot(plots)) {
    if (is.null(names)) {
      filename <- file.path(dir, paste0("\\", Sys.Date(), "_Plot.png"))
    } else {
      # Ensure .png extension
      filename <- file.path(dir, ifelse(grepl("\\.png$", names), names,
                                        paste0(names, ".png")))
    }

    save_single_plot(plots, filename, ...)
    message("Plot saved to: ", filename)

    # Case 2: List of plots
  } else if (is.list(plots) && length(plots) > 0L) {

    # Validate that all elements are ggplots
    if (!all(sapply(plots, is.ggplot))) {
      stop("All elements in 'plots' list must be ggplot objects", call. = FALSE)
    }

    # Handle names
    if (is.null(names)) {

      # Generate default names
      plot_names <- paste0("Plot_", seq_along(plots))
    } else if (length(names) == 1L) {

      # Single name - append numbers
      plot_names <- paste0(names, "_", seq_along(plots))
    } else if (length(names) == length(plots)) {

      # Names match number of plots
      plot_names <- names
    } else {
      stop("Length of 'names' must be 1 or equal to the number of plots (",
           length(plots), ")", call. = FALSE)
    }

    # Save each plot
    filenames <- character(length(plots))
    for (i in seq_along(plots)) {

      # Ensure .png extension
      plot_name <- ifelse(grepl("\\.png$", plot_names[i]),
                          plot_names[i], paste0(plot_names[i], ".png"))

      filenames[i] <- file.path(dir, paste0(Sys.Date(), "_", plot_name))
      save_single_plot(plots[[i]], filenames[i], ...)

      }

    message("Plots saved to: ", paste(filenames, collapse = ", "))

  } else {
    stop("'plots' must be either a single ggplot object or a non-empty list of
         ggplot objects",
         call. = FALSE)
  }

  invisible(NULL)

}
