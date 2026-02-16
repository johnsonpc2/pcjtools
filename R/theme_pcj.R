#' Custom Theme
#'
#' A function to create and save consistent plots with aesthetics chosen to
#' enhance accessibility
#'
#' @param gridlines Logical, defaults to TRUE. Should gridlines be generated
#'  behind plotted data?
#' @inheritParams theme_pcj_palettes
#' @param font The font to be used in a plot. Defaults to Atkinson Hyperlegible.
#' @inheritDotParams ggplot2::theme
#'
#' @returns A plot object with the specified aesthetics.
#' @export
#'
#' @examples
#' \donttest{
#' # Create a basic plot
#' g1 <- ggplot2::ggplot(
#'   data = mtcars,
#'   ggplot2::aes(x = mpg, y = wt, color = factor(cyl))
#' ) +
#'   ggplot2::geom_point() +
#'   ggplot2::facet_wrap(~factor(cyl))
#'
#' # Apply custom theme
#' g1 + theme_pcj(font = "sans")
#' }

theme_pcj <- function(gridlines = TRUE, palette = "default",
                      font = "Atkinson Hyperlegible", ...) {

  `%+replace%` <- ggplot2::`%+replace%`

  font_install(font = font)

  colors <- c('#000000', '#131619', '#1d252b', '#29353d', '#354651', '#425765',
              '#506879', '#607a8d', '#708da1', '#82a0b5', '#95b3c8', '#abc6da',
              '#c3daea', '#deedf8', '#ffffff')

  # Capture the ... arguments
  dots <- list(...)

  # Set gridlines based on argument
  gridlines <- if (isTRUE(gridlines)) {
    ggplot2::element_line(
      color = colors[6],
      linewidth = .25,
      linetype = "dashed"
    )
  } else {
    ggplot2::element_blank()
  }

  # Set defaults for all theme parameters (only if not provided by user)
  if (!"line" %in% names(dots)) {
    dots$line <- ggplot2::element_line(color = colors[1], linewidth = .5)
  }

  if (!"text" %in% names(dots)) {
    dots$text <- ggplot2::element_text(
      family = font,
      size = 16,
      face = "bold",
      lineheight = 1.2,
      margin = ggplot2::margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")
    )
  }

  if (!"axis.line" %in% names(dots)) {
    dots$axis.line <- ggplot2::element_line(color = colors[1], linewidth = 0.5)
  }

  if (!"axis.text" %in% names(dots)) {
    dots$axis.text <- ggplot2::element_text(size = ggplot2::rel(1.2), color = colors[3])
  }

  if (!"axis.text.y" %in% names(dots)) {
    dots$axis.text.y <- ggplot2::element_text(
      margin = ggplot2::margin(t = 0, r = 5, b = 0, l = 0)
    )
  }

  if (!"axis.text.x" %in% names(dots)) {
    dots$axis.text.x <- ggplot2::element_text(
      margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0)
    )
  }

  if (!"axis.ticks" %in% names(dots)) {
    dots$axis.ticks <- ggplot2::element_line(color = colors[1], linewidth = 0.5)
  }

  if (!"axis.ticks.length" %in% names(dots)) {
    dots$axis.ticks.length <- ggplot2::unit(3.5, "pt")
  }

  if (!"axis.title" %in% names(dots)) {
    dots$axis.title <- ggplot2::element_text(
      size = ggplot2::rel(1.5),
      color = colors[1]
    )
  }

  if (!"axis.title.y" %in% names(dots)) {
    dots$axis.title.y <- ggplot2::element_text(
      angle = 90,
      margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0, unit = "pt")
    )
  }

  if (!"axis.title.x" %in% names(dots)) {
    dots$axis.title.x <- ggplot2::element_text(
      angle = 0,
      margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")
    )
  }

  if (!"legend.box.background" %in% names(dots)) {
    dots$legend.box.background <- ggplot2::element_rect(linewidth = 1)
  }

  if (!"legend.box.margin" %in% names(dots)) {
    dots$legend.box.margin <- ggplot2::margin(t = 5, r = 25, b = 0, l = 25)
  }

  if (!"legend.box.spacing" %in% names(dots)) {
    dots$legend.box.spacing <- ggplot2::unit(6, "pt")
  }

  if (!"legend.direction" %in% names(dots)) {
    dots$legend.direction <- "horizontal"
  }

  if (!"legend.justification" %in% names(dots)) {
    dots$legend.justification <- c(1, 0)
  }

  if (!"legend.key.size" %in% names(dots)) {
    dots$legend.key.size <- ggplot2::unit(20, "pt")
  }

  if (!"legend.position" %in% names(dots)) {
    dots$legend.position <- c(0.95, 1.035)
  }

  if (!"legend.text" %in% names(dots)) {
    dots$legend.text <- ggplot2::element_text(
      size = ggplot2::rel(1.2),
      color = colors[6],
      margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0)
    )
  }

  if (!"legend.text.position" %in% names(dots)) {
    dots$legend.text.position <- "bottom"
  }

  if (!"legend.title" %in% names(dots)) {
    dots$legend.title <- ggplot2::element_text(
      hjust = 0.5,
      margin = ggplot2::margin(t = 0, r = 0, b = 1, l = 0, unit = "pt")
    )
  }

  if (!"legend.title.position" %in% names(dots)) {
    dots$legend.title.position <- "top"
  }

  if (!"panel.grid.major" %in% names(dots)) {
    dots$panel.grid.major <- gridlines
  }

  if (!"panel.grid.minor" %in% names(dots)) {
    dots$panel.grid.minor <- ggplot2::element_blank()
  }

  if (!"panel.spacing" %in% names(dots)) {
    dots$panel.spacing <- ggplot2::unit(.5, units = "in")
  }

  if (!"plot.background" %in% names(dots)) {
    dots$plot.background <- ggplot2::element_rect(fill = colors[15])
  }

  if (!"plot.caption" %in% names(dots)) {
    dots$plot.caption <- ggplot2::element_text(color = colors[11], hjust = 1)
  }

  if (!"plot.caption.position" %in% names(dots)) {
    dots$plot.caption.position <- "plot"
  }

  if (!"plot.margin" %in% names(dots)) {
    dots$plot.margin <- ggplot2::margin(t = .15, r = .25, b = .15, l = .25, unit = "in")
  }

  if (!"plot.title" %in% names(dots)) {
    dots$plot.title <- ggplot2::element_text(
      color = colors[1],
      size = ggplot2::rel(2.5),
      hjust = 0,
      margin = ggplot2::margin(t = 0, r = 0, b = 15, l = 0, unit = "pt")
    )
  }

  if (!"plot.title.position" %in% names(dots)) {
    dots$plot.title.position <- "plot"
  }

  if (!"plot.subtitle" %in% names(dots)) {
    dots$plot.subtitle <- ggplot2::element_text(
      size = ggplot2::rel(2),
      color = colors[9],
      hjust = 0,
      margin = ggplot2::margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")
    )
  }

  if (!"strip.background" %in% names(dots)) {
    dots$strip.background <- ggplot2::element_rect(fill = colors[13])
  }

  if (!"strip.placement" %in% names(dots)) {
    dots$strip.placement <- "inside"
  }

  # Apply the theme with all defaults and user overrides
  ggplot2::theme_void() %+replace%
    do.call(ggplot2::theme, dots)

}
