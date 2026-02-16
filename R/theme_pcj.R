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

  # ========== BASE ELEMENTS ==========
  if (!"line" %in% names(dots)) {
    dots$line <- ggplot2::element_line(color = colors[1], linewidth = .5)
  }

  if (!"rect" %in% names(dots)) {
    dots$rect <- ggplot2::element_rect(fill = colors[15], color = colors[1], linewidth = 0.5)
  }

  if (!"text" %in% names(dots)) {
    dots$text <- ggplot2::element_text(
      family = font,
      size = 16,
      face = "bold",
      lineheight = 1.2,
      margin = ggplot2::margin(t = 0, r = 0, b = 5, l = 0, unit = "pt"),
      color = colors[1]
    )
  }

  # ========== AXIS ELEMENTS ==========
  if (!"axis.line" %in% names(dots)) {
    dots$axis.line <- ggplot2::element_line(color = colors[1], linewidth = 0.5)
  }

  if (!"axis.line.x" %in% names(dots)) {
    dots$axis.line.x <- NULL  # Inherits from axis.line
  }

  if (!"axis.line.y" %in% names(dots)) {
    dots$axis.line.y <- NULL  # Inherits from axis.line
  }

  if (!"axis.text" %in% names(dots)) {
    dots$axis.text <- ggplot2::element_text(size = ggplot2::rel(1.2), color = colors[3])
  }

  if (!"axis.text.x" %in% names(dots)) {
    dots$axis.text.x <- ggplot2::element_text(
      margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0)
    )
  }

  if (!"axis.text.x.top" %in% names(dots)) {
    dots$axis.text.x.top <- NULL  # Inherits from axis.text.x
  }

  if (!"axis.text.x.bottom" %in% names(dots)) {
    dots$axis.text.x.bottom <- NULL  # Inherits from axis.text.x
  }

  if (!"axis.text.y" %in% names(dots)) {
    dots$axis.text.y <- ggplot2::element_text(
      margin = ggplot2::margin(t = 0, r = 5, b = 0, l = 0)
    )
  }

  if (!"axis.text.y.left" %in% names(dots)) {
    dots$axis.text.y.left <- NULL  # Inherits from axis.text.y
  }

  if (!"axis.text.y.right" %in% names(dots)) {
    dots$axis.text.y.right <- NULL  # Inherits from axis.text.y
  }

  if (!"axis.ticks" %in% names(dots)) {
    dots$axis.ticks <- ggplot2::element_line(color = colors[1], linewidth = 0.5)
  }

  if (!"axis.ticks.x" %in% names(dots)) {
    dots$axis.ticks.x <- NULL  # Inherits from axis.ticks
  }

  if (!"axis.ticks.x.top" %in% names(dots)) {
    dots$axis.ticks.x.top <- NULL  # Inherits from axis.ticks.x
  }

  if (!"axis.ticks.x.bottom" %in% names(dots)) {
    dots$axis.ticks.x.bottom <- NULL  # Inherits from axis.ticks.x
  }

  if (!"axis.ticks.y" %in% names(dots)) {
    dots$axis.ticks.y <- NULL  # Inherits from axis.ticks
  }

  if (!"axis.ticks.y.left" %in% names(dots)) {
    dots$axis.ticks.y.left <- NULL  # Inherits from axis.ticks.y
  }

  if (!"axis.ticks.y.right" %in% names(dots)) {
    dots$axis.ticks.y.right <- NULL  # Inherits from axis.ticks.y
  }

  if (!"axis.ticks.length" %in% names(dots)) {
    dots$axis.ticks.length <- ggplot2::unit(3.5, "pt")
  }

  if (!"axis.ticks.length.x" %in% names(dots)) {
    dots$axis.ticks.length.x <- NULL  # Inherits from axis.ticks.length
  }

  if (!"axis.ticks.length.x.top" %in% names(dots)) {
    dots$axis.ticks.length.x.top <- NULL
  }

  if (!"axis.ticks.length.x.bottom" %in% names(dots)) {
    dots$axis.ticks.length.x.bottom <- NULL
  }

  if (!"axis.ticks.length.y" %in% names(dots)) {
    dots$axis.ticks.length.y <- NULL  # Inherits from axis.ticks.length
  }

  if (!"axis.ticks.length.y.left" %in% names(dots)) {
    dots$axis.ticks.length.y.left <- NULL
  }

  if (!"axis.ticks.length.y.right" %in% names(dots)) {
    dots$axis.ticks.length.y.right <- NULL
  }

  if (!"axis.title" %in% names(dots)) {
    dots$axis.title <- ggplot2::element_text(
      size = ggplot2::rel(1.5),
      color = colors[1]
    )
  }

  if (!"axis.title.x" %in% names(dots)) {
    dots$axis.title.x <- ggplot2::element_text(
      angle = 0,
      margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")
    )
  }

  if (!"axis.title.x.top" %in% names(dots)) {
    dots$axis.title.x.top <- NULL  # Inherits from axis.title.x
  }

  if (!"axis.title.x.bottom" %in% names(dots)) {
    dots$axis.title.x.bottom <- NULL  # Inherits from axis.title.x
  }

  if (!"axis.title.y" %in% names(dots)) {
    dots$axis.title.y <- ggplot2::element_text(
      angle = 90,
      margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0, unit = "pt")
    )
  }

  if (!"axis.title.y.left" %in% names(dots)) {
    dots$axis.title.y.left <- NULL  # Inherits from axis.title.y
  }

  if (!"axis.title.y.right" %in% names(dots)) {
    dots$axis.title.y.right <- NULL  # Inherits from axis.title.y
  }

  # ========== LEGEND ELEMENTS ==========
  if (!"legend.background" %in% names(dots)) {
    dots$legend.background <- ggplot2::element_rect(fill = colors[15], color = NA)
  }

  if (!"legend.margin" %in% names(dots)) {
    dots$legend.margin <- ggplot2::margin(t = 0, r = 0, b = 0, l = 0)
  }

  if (!"legend.spacing" %in% names(dots)) {
    dots$legend.spacing <- ggplot2::unit(10, "pt")
  }

  if (!"legend.spacing.x" %in% names(dots)) {
    dots$legend.spacing.x <- NULL  # Inherits from legend.spacing
  }

  if (!"legend.spacing.y" %in% names(dots)) {
    dots$legend.spacing.y <- NULL  # Inherits from legend.spacing
  }

  if (!"legend.key" %in% names(dots)) {
    dots$legend.key <- ggplot2::element_rect(fill = colors[15], color = NA)
  }

  if (!"legend.key.size" %in% names(dots)) {
    dots$legend.key.size <- ggplot2::unit(20, "pt")
  }

  if (!"legend.key.height" %in% names(dots)) {
    dots$legend.key.height <- NULL  # Inherits from legend.key.size
  }

  if (!"legend.key.width" %in% names(dots)) {
    dots$legend.key.width <- NULL  # Inherits from legend.key.size
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

  if (!"legend.position" %in% names(dots)) {
    dots$legend.position <- c(0.95, 1.035)
  }

  if (!"legend.direction" %in% names(dots)) {
    dots$legend.direction <- "horizontal"
  }

  if (!"legend.justification" %in% names(dots)) {
    dots$legend.justification <- c(1, 0)
  }

  if (!"legend.box" %in% names(dots)) {
    dots$legend.box <- NULL  # Default is "vertical"
  }

  if (!"legend.box.just" %in% names(dots)) {
    dots$legend.box.just <- NULL  # Default is "top"
  }

  if (!"legend.box.margin" %in% names(dots)) {
    dots$legend.box.margin <- ggplot2::margin(t = 5, r = 15, b = 5, l = 15)
  }

  if (!"legend.box.background" %in% names(dots)) {
    dots$legend.box.background <- ggplot2::element_rect(linewidth = 1)
  }

  if (!"legend.box.spacing" %in% names(dots)) {
    dots$legend.box.spacing <- ggplot2::unit(6, "pt")
  }

  # ========== PANEL ELEMENTS ==========
  if (!"panel.background" %in% names(dots)) {
    dots$panel.background <- ggplot2::element_rect(fill = colors[15], color = NA)
  }

  if (!"panel.border" %in% names(dots)) {
    dots$panel.border <- ggplot2::element_blank()
  }

  if (!"panel.spacing" %in% names(dots)) {
    dots$panel.spacing <- ggplot2::unit(.5, units = "in")
  }

  if (!"panel.spacing.x" %in% names(dots)) {
    dots$panel.spacing.x <- NULL  # Inherits from panel.spacing
  }

  if (!"panel.spacing.y" %in% names(dots)) {
    dots$panel.spacing.y <- NULL  # Inherits from panel.spacing
  }

  if (!"panel.grid" %in% names(dots)) {
    dots$panel.grid <- NULL  # Inherits from panel.grid.major and panel.grid.minor
  }

  if (!"panel.grid.major" %in% names(dots)) {
    dots$panel.grid.major <- gridlines
  }

  if (!"panel.grid.major.x" %in% names(dots)) {
    dots$panel.grid.major.x <- NULL  # Inherits from panel.grid.major
  }

  if (!"panel.grid.major.y" %in% names(dots)) {
    dots$panel.grid.major.y <- NULL  # Inherits from panel.grid.major
  }

  if (!"panel.grid.minor" %in% names(dots)) {
    dots$panel.grid.minor <- ggplot2::element_blank()
  }

  if (!"panel.grid.minor.x" %in% names(dots)) {
    dots$panel.grid.minor.x <- NULL  # Inherits from panel.grid.minor
  }

  if (!"panel.grid.minor.y" %in% names(dots)) {
    dots$panel.grid.minor.y <- NULL  # Inherits from panel.grid.minor
  }

  if (!"panel.ontop" %in% names(dots)) {
    dots$panel.ontop <- FALSE
  }

  # ========== PLOT ELEMENTS ==========
  if (!"plot.background" %in% names(dots)) {
    dots$plot.background <- ggplot2::element_rect(fill = colors[15], color = NA)
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

  if (!"plot.caption" %in% names(dots)) {
    dots$plot.caption <- ggplot2::element_text(color = colors[11], hjust = 1)
  }

  if (!"plot.caption.position" %in% names(dots)) {
    dots$plot.caption.position <- "plot"
  }

  if (!"plot.tag" %in% names(dots)) {
    dots$plot.tag <- ggplot2::element_text(
      size = ggplot2::rel(1.5),
      hjust = 0.5,
      vjust = 0.5
    )
  }

  if (!"plot.tag.position" %in% names(dots)) {
    dots$plot.tag.position <- "topleft"
  }

  if (!"plot.margin" %in% names(dots)) {
    dots$plot.margin <- ggplot2::margin(t = .15, r = .25, b = .15, l = .25, unit = "in")
  }

  # ========== STRIP ELEMENTS (for facets) ==========
  if (!"strip.background" %in% names(dots)) {
    dots$strip.background <- ggplot2::element_rect(fill = colors[13], color = NA)
  }

  if (!"strip.background.x" %in% names(dots)) {
    dots$strip.background.x <- NULL  # Inherits from strip.background
  }

  if (!"strip.background.y" %in% names(dots)) {
    dots$strip.background.y <- NULL  # Inherits from strip.background
  }

  if (!"strip.clip" %in% names(dots)) {
    dots$strip.clip <- "inherit"
  }

  if (!"strip.placement" %in% names(dots)) {
    dots$strip.placement <- "inside"
  }

  if (!"strip.text" %in% names(dots)) {
    dots$strip.text <- ggplot2::element_text(
      size = ggplot2::rel(1.2),
      color = colors[1],
      margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5)
    )
  }

  if (!"strip.text.x" %in% names(dots)) {
    dots$strip.text.x <- NULL  # Inherits from strip.text
  }

  if (!"strip.text.x.top" %in% names(dots)) {
    dots$strip.text.x.top <- NULL  # Inherits from strip.text.x
  }

  if (!"strip.text.x.bottom" %in% names(dots)) {
    dots$strip.text.x.bottom <- NULL  # Inherits from strip.text.x
  }

  if (!"strip.text.y" %in% names(dots)) {
    dots$strip.text.y <- NULL  # Inherits from strip.text
  }

  if (!"strip.text.y.left" %in% names(dots)) {
    dots$strip.text.y.left <- NULL  # Inherits from strip.text.y
  }

  if (!"strip.text.y.right" %in% names(dots)) {
    dots$strip.text.y.right <- NULL  # Inherits from strip.text.y
  }

  if (!"strip.switch.pad.grid" %in% names(dots)) {
    dots$strip.switch.pad.grid <- ggplot2::unit(3, "pt")
  }

  if (!"strip.switch.pad.wrap" %in% names(dots)) {
    dots$strip.switch.pad.wrap <- ggplot2::unit(3, "pt")
  }

  # ========== OTHER ELEMENTS ==========
  if (!"aspect.ratio" %in% names(dots)) {
    dots$aspect.ratio <- NULL  # No fixed aspect ratio by default
  }

  # Apply the theme with all defaults and user overrides
  ggplot2::theme_void() %+replace%
    do.call(ggplot2::theme, dots)
}
