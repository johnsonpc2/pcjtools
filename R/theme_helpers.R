#' a helper function used by 'theme_pcj()' to make consistently themed plots
#'
#' @param base_size an integer. The graph's default font size.
#' @param dark_text a quoted hex string. Sets the color of the darkest text in a
#' plot. All text is based on shades of the specified hex code.
#' @param font a quoted name of the font you would like the plot's text to be
#' printed in
#' @param ... additional arguments passed to 'theme()'
#'
#' @import ggplot2
#'
#' @keywords internal
#'
#' @returns a plot configured with aesthetics reflecting the specified settings
#'
#' @examples
#' \dontrun{
#' ggplot2::ggplot(data = mtcars, ggplot2::aes(x = wt, y = mpg, color = gear)) +
#' ggplot2::geom_point() +
#' theme_pcj_aesthetics(base_size = 12, dark_text = "#000000")
#' }

theme_pcj_aesthetics <- function(base_size,
                                 dark_text,
                                 font,
                                 ...) {

  mid_text <- monochromeR::generate_palette(
    colour = dark_text,
    modification = "go_lighter",
    n_colours = 9
  )[4]

  light_text <- monochromeR::generate_palette(
    colour = dark_text,
    modification = "go_lighter",
    n_colours = 9
  )[7]

  if (!font %in% systemfonts::system_fonts()$family) {
    font <- "sans"  # fallback to system font
  }

  theme_minimal() %+replace%
    theme(
      text = element_text(
        family = font,
        size = base_size,
        face = "bold",
        lineheight = 1.1,
        margin = margin(t = 0, r = 0, b = 10, l = 0)
      ),
      plot.title = element_text(
        color = dark_text,
        size = rel(2),
        hjust = 0
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.subtitle = element_text(
        size = rel(1.5),
        color = mid_text,
        hjust = 0
      ),
      axis.text = element_text(
        size = rel(1.0),
        color = dark_text
      ),
      axis.title = element_text(
        size = rel(1.2),
        color = dark_text
      ),
      panel.grid = element_line(
        color = mid_text,
        linewidth = .15,
        linetype = "dashed"
      ),
      plot.caption = element_text(
        color = light_text,
        hjust = 1
      ),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.text = element_text(
        size = rel(1.2),
        lineheight = 1,
        color = mid_text
      ),
      legend.direction = "horizontal",
      ...
    )

}



#' a helper function defining the palettes accessible to 'theme_pcj()'
#'
#' @param palette a string. The name of the palette to be mapped to a variable,
#' including: "default", "neg_to_pos", "mono_printing", "mono_blue", "mono_red",
#' "mono_yellow", or "ualbany".
#' @param continuous logical. Is the variable continuous or discrete?
#' @param .colors loads the colors defined within pcj_colors.
#' @param .palettes loads the palettes defined within pcj_palettes.
#'
#' @keywords internal
#'
#' @returns a plot object with the specified aesthetics rendered.
#'
#' @examples
#' \dontrun{
#' pcj_graph_palettes(palette = "default")
#' pcj_graph_palettes(palette = "mono_printing")
#' }

theme_pcj_palettes <- function(palette, continuous,
                               .colors = pcj_colors,
                               .palettes = pcj_palettes) {
  # Define colors
  pcj_colors <- list(
    black = "#000000",
    cyan = "#88CCEE",
    dark_blue = "#004488",
    dark_red = "#994455",
    dark_yellow = "#997700",
    green = "#117733",
    indigo = "#332288",
    teal = "#44AA99",
    light_blue = "#6699CC",
    light_red = "#EE99AA",
    light_yellow = "#EECC66",
    olive = "#999933",
    pale_gray = "#DDDDDD",
    purple = "#AA4499",
    rose = "#CC6677",
    sand = "#DDCC77",
    white = "#FFFFFF",
    wine = "#882255",
    gold = "#eeb211",
    purple_dark = "#46166b",
    gray = "#a2aaad"
  )

  # Combine colors into different palettes
  pcj_palettes <- list(
    default = c(
      pcj_colors$cyan,
      pcj_colors$green,
      pcj_colors$indigo,
      pcj_colors$teal,
      pcj_colors$olive,
      pcj_colors$pale_gray,
      pcj_colors$purple,
      pcj_colors$rose,
      pcj_colors$sand,
      pcj_colors$wine
    ),
    neg_to_pos = c(
      pcj_colors$wine,
      pcj_colors$sand
    ),
    mono_printing = c(
      pcj_colors$white,
      pcj_colors$light_yellow,
      pcj_colors$light_red,
      pcj_colors$light_blue,
      pcj_colors$dark_yellow,
      pcj_colors$dark_red,
      pcj_colors$dark_blue,
      pcj_colors$black
    ),
    mono_blue = c(
      pcj_colors$light_blue,
      pcj_colors$dark_blue
    ),
    mono_red = c(
      pcj_colors$light_red,
      pcj_colors$dark_red
    ),
    mono_yellow = c(
      pcj_colors$light_yellow,
      pcj_colors$dark_yellow
    ),
    ualbany = c(
      pcj_colors$gold,
      pcj_colors$purple_dark,
      pcj_colors$black,
      pcj_colors$gray
    )
  )

  if (continuous == FALSE) {

    ggplot2::discrete_scale(
      palette = grDevices::colorRampPalette(.palettes[[palette]]),
      aesthetics = c("color", "fill"),
      na.value = .colors$na_value
    )

  } else {

    ggplot2::scale_color_gradientn(
      colors = .palettes[[palette]],
      na.value = .colors$na_value
    )

  }
}



#' a function to apply text to a plot
#'
#' @param plot_text a named list of plot features and their text label
#' @param alt_text logical. Should a subtitle and caption be generated for the
#' plot?
#'
#' @keywords internal
#'
#' @returns a ggplot object with the rendered text specified in the function
#' call
#'
#' @examples
#' \dontrun{
#' ggplot2::ggplot(data = mtcars, ggplot2::aes(x = wt, y = mpg, color = gear)) +
#'   ggplot2::geom_point() +
#'   theme_pcj_text()
#'   }

theme_pcj_text <- function(
    plot_text = c("title", "subtitle", "xlab", "ylab",
      paste0("Rendered:", format(Sys.time(), "%Y%m%d, %H:%M"))
    ),
    alt_text = FALSE) {

  label_names <- plot_text

  if (alt_text == TRUE) {

    labs(
      title = if (!is.na(label_names["title"])) {
        label_names["title"]
      } else {
              NULL},
      subtitle = if (!is.na(label_names["subtitle"])) {
        label_names["subtitle"]
      } else {
              NULL},
      x = if (!is.na(label_names["xlab"])) {
        label_names["xlab"]
      } else {
              NULL},
      y = if (!is.na(label_names["ylab"])) {
        label_names["ylab"]
      } else {
              NULL},
      caption = if (!is.na(label_names["caption"])) {
        label_names["caption"]
      } else {
        paste0("Rendered: ", format(Sys.time(), "%Y%m%d, %H:%M"))
      }
    )

  } else {

    labs(
      title = if (!is.na(label_names["title"])) {
        label_names["title"]
      } else {
              NULL},
      subtitle = NULL,
      x = if (!is.na(label_names["xlab"])) {
        label_names["xlab"]
      } else {
              NULL},
      y = if (!is.na(label_names["ylab"])) {
        label_names["ylab"]
      } else {
              NULL},
      caption = NULL
    )
  }
}
