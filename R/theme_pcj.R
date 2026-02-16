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

  border <- ggplot2::element_rect(
    color = colors[1],
    fill = NA,
    linewidth = 0.10
  )

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

  ggplot2::theme_void() %+replace%
    ggplot2::theme(
      line = ggplot2::element_line(
        color = colors[1],
        linewidth = .5
      ),
      text = ggplot2::element_text(
        family = font,
        size = 16,
        face = "bold",
        lineheight = 1.2,
        margin = ggplot2::margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")
      ),
      axis.line = ggplot2::element_line(
        color = colors[1],
        linewidth = 0.5
      ),
      axis.text = ggplot2::element_text(
        size = ggplot2::rel(1.2),
        color = colors[3]
      ),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = 5, b = 0, l = 0)
      ),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0)
      ),
      axis.ticks = ggplot2::element_line(
        color = colors[1],
        linewidth = 0.5
      ),
      axis.ticks.length = ggplot2::unit(3.5, "pt"),
      axis.title = ggplot2::element_text(
        size = ggplot2::rel(1.5),
        color = colors[1]
      ),
      axis.title.y = ggplot2::element_text(
        angle = 90,
        margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0, unit = "pt")
      ),
      axis.title.x = ggplot2::element_text(
        angle = 0,
        margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")
      ),
      legend.box.background = ggplot2::element_rect(
        linewidth = 1
      ),
      legend.box.margin = ggplot2::margin(t = 5, r = 25, b = 0, l = 25),
      legend.box.spacing = ggplot2::unit(6, "pt"),
      legend.direction = "horizontal",
      legend.justification = c(1, 0),
      legend.key.size = ggplot2::unit(20, "pt"),
      legend.position = c(0.95, 1.035), # (horizontal, vertical)
      legend.text = ggplot2::element_text(
        size = ggplot2::rel(1.2),
        color = colors[6],
        margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0)
      ),
      legend.text.position = "bottom",
      legend.title = ggplot2::element_text(
        hjust = 0.5,
        margin = ggplot2::margin(t = 0, r = 0, b = 1, l = 0, unit = "pt")
      ),
      legend.title.position = "top",
      panel.grid.major = gridlines,
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(.5, units = "in"),
      plot.background = ggplot2::element_rect(fill = colors[15]),
      plot.caption = ggplot2::element_text(
        color = colors[11],
        hjust = 1
      ),
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(t = .15, r = .25, b = .15, l = .25, unit = "in"),
      plot.title = ggplot2::element_text(
        color = colors[1],
        size = ggplot2::rel(2.5),
        hjust = 0,
        margin = ggplot2::margin(t = 0, r = 0, b = 15, l = 0, unit = "pt")
      ),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(2),
        color = colors[9],
        hjust = 0,
        margin = ggplot2::margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")
      ),
      strip.background = ggplot2::element_rect(
        fill = colors[13]
      ),
      strip.placement = "inside",
      ...
    )
}
