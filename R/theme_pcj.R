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
#' @export
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#'
#' # Create a basic plot
#' g1 <- ggplot(
#'   data = mtcars,
#'   aes(x = mpg, y = wt, color = factor(cyl))
#' ) +
#'   geom_line(linewidth = 2)
#'
#' # Apply custom theme
#' thm <- theme_pcj(
#'   plot = g1,
#'   font = "sans",
#'   plot_text = c(
#'     title = "Car Weight vs MPG",
#'     xlab = "Miles per Gallon",
#'     ylab = "Weight (1000 lbs)"
#'   )
#' )
#'
#' # Display the plot
#' print(thm)
#' }

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
            is.null(base_size) || (is.numeric(base_size) && length(base_size) == 1L),  # Changed this line
            is.character(dark_text) && length(dark_text) == 1L,
            is.character(font) && length(font) == 1L,
            is.character(palette) && length(palette) == 1L,
            is.logical(continuous) && length(continuous) == 1L,
            is.character(plot_text) && length(plot_text <= 5L))

  # Check if Atkinson Hyperlegible is registered
  available_fonts <- systemfonts::system_fonts()$family

  if ("Atkinson Hyperlegible" %in% available_fonts) {
    base_family <- "Atkinson Hyperlegible"
  } else {
    # Try to register from bundled fonts if not already available
    font_dir <- system.file("fonts", package = "pcjtools")
    if (dir.exists(font_dir)) {
      # Attempt registration (in case .onLoad didn't work)
      tryCatch({
        font_files <- list.files(font_dir, pattern = "\\.ttf$|\\.otf$",
                                 full.names = TRUE, ignore.case = TRUE)
        regular <- grep("regular", font_files, ignore.case = TRUE, value = TRUE)[1]
        if (!is.na(regular)) {
          systemfonts::register_font(
            name = "Atkinson Hyperlegible",
            plain = regular
          )
          base_family <- "Atkinson Hyperlegible"
        } else {
          base_family <- "sans"
        }
      }, error = function(e) {
        base_family <- "sans"
      })
    } else {
      base_family <- "sans"
    }
  }

  filename <- paste0(save_path, format(Sys.time(), "%Y%m%d_"),
                    plot_text["title"], ".png")

  modified_plot <- plot +
    theme_pcj_aesthetics(base_size = base_size,
                         dark_text = dark_text,
                         font = font, ...) +
    theme_pcj_palettes(palette = palette,
                       continuous = continuous) +
    theme_pcj_text(plot_text = plot_text,
                   alt_text = alt_text)

  if (!is.null(save_path)) {

    plot_saver(plots = modified_plot, dir = save_path)

  }

  return(modified_plot)

}
