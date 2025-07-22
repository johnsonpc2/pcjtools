#' Custom Theme
#'
#' A function to create and save consistent plots with aesthetics chosen to
#' enhance accessibility
#'
#' @param plot A ggplot object to be formatted.
#' @inheritParams theme_pcj_aesthetics
#' @inheritParams theme_pcj_palettes
#' @inheritParams theme_pcj_text
#' @param save_path A string. The directory path where the plot should be saved.
#' @param ... Additional arguments passed to `theme()` or `ggsave()`.
#'
#' @returns A plot object with the specified aesthetics.
#'
#' @export
#'
#' @examples
#' g1 <- ggplot2::ggplot(
#' data = mtcars,
#' ggplot2::aes(x = mpg, y = wt, color = factor(cyl))
#' ) +
#' ggplot2::geom_line(linewidth = 2)
#'
#' thm <- theme_pcj(plot = g1, font = "sans")

theme_pcj <- function(plot, base_size = 12, dark_text = "#000000",
                      font = "Atkinson Hyperlegible", palette = "default",
                      continuous = FALSE,
                      plot_text = c(title = "title", subtitle = "subtitle",
                                    xlab = "xlab", ylab = "ylab",
                                    caption = paste0("Created: ",
                                                     format(Sys.time(),
                                                            "%Y%m%d, %H:%M"))),
                      alt_text = TRUE, save_path = NULL, ...) {

  stopifnot(is_ggplot(plot),
            if (!is.null(base_size)) {
              is.numeric(base_size) && length(base_size) == 1L
              } else {break},
            is.character(dark_text) && length(dark_text) == 1L,
            is.character(font) && length(font) == 1L,
            is.character(palette) && length(palette) == 1L,
            is.logical(continuous) && length(continuous) == 1L,
            is.character(plot_text) && length(plot_text <= 5L))

  filename <- paste0(save_path, format(Sys.time(), "%Y%m%d_"),
                    plot_text["title"], ".png")

  dots <- list(...)

  modified_plot <- plot +
    theme_pcj_aesthetics(base_size = base_size,
                         dark_text = dark_text,
                         font, ...) +
    theme_pcj_palettes(palette = palette,
                       continuous = continuous) +
    theme_pcj_text(plot_text = plot_text,
                   alt_text = alt_text)

  if (!is.null(save_path)) {

    plot_saver(plots = modified_plot, dir = save_path)

  }

  invisible(x = modified_plot)

}
