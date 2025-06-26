#' a helper function to install the 'Atkinson Hyperlegible' font, used by
#' 'theme_pcj()'
#'
#' @import ragg
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' install_atkinson()

install_atkinson <- function() {

  path <- system.file("fonts", package = "pcjtools")

  extrafont::ttf_import(paths = path,
                        recursive = FALSE,
                        pattern = "Atkinson")

  extrafont::loadfonts(quiet = TRUE)
}



#' a helper function defining theme elements to make consistent plots and graphs
#' with 'theme_pcj()'
#'
#' @param base_size an integer. The graph's default font size.
#' @param dark_text a quoted hex string. Sets the color of the darkest text in a
#' plot. All text is based on shades of the specified hex code.
#' @param ... additional arguments passed to 'theme()'
#' @import ggplot2
#'
#' @returns a plot configured with aesthetics reflecting the specified settings
#'
#' @examples
#' ggplot2::ggplot(data = mtcars, ggplot2::aes(x = wt, y = mpg, color = gear)) +
#' ggplot2::geom_point() +
#' theme_pcj_aesthetics()

theme_pcj_aesthetics <- function(base_size,
                                 dark_text,
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

  theme_minimal() %+replace%
    theme(
      text = element_text(
        family = "Atkinson Hyperlegible",
        size = base_size,
        face = "bold",
        lineheight = 1.1,
        margin = margin(0, 0, 6, 0)
      ),
      plot.title = element_text(
        color = dark_text,
        size = rel(2),
      ),
      plot.subtitle = element_text(
        size = rel(1.5),
        color = mid_text,
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
      legend.title = element_blank(),
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



#' a helper function defining the palettes accessible to `theme_pcj()`
#'
#' @param palette a string. The name of the palette to be mapped to a variable.
#' @param continuous logical. Is the variable continuous or discrete?
#' @param .colors loads the colors defined within pcj_colors.
#' @param .palettes loads the palettes defined within pcj_palettes.
#'
#' @import ggplot2
#'
#' @returns a plot object with the specified aesthetics rendered
#'
#' @examples
#' ggplot2::ggplot(data = mtcars, ggplot2::aes(x = wt, y = mpg, color = gear)) +
#' ggplot2::geom_point() +
#' theme_pcj_palettes()

theme_pcj_palettes <- function(palette = "default",
                               continuous = FALSE,
                               .colors = .colors,
                               .palettes = .palettes) {
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

  # Combine colors into palettes
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

  # Use local definitions if not provided
  if (is.null(.colors)) .colors <- pcj_colors
  if (is.null(.palettes)) .palettes <- pcj_palettes

  # If the scale the variable 'color' is mapped to is discrete...
  if (continuous == FALSE) {

    discrete_scale(
      palette = grDevices::colorRampPalette(.palettes[[palette]]),
      aesthetics = c("color", "fill"),
      na.value = .colors$na_value
    )

    # Otherwise...
  } else {

    scale_color_gradientn(
      colors = .palettes[[palette]],
      na.value = .colors$na_value
    )
  }
}



#' a function to apply text to a plot
#'
#' @param text a named list of plot features and their text label
#' @param alt_text logical. Should a subtitle and caption be generated for the
#' plot?
#'
#' @returns a ggplot object with the rendered text specified in the function
#' call
#'
#' @examples
#' ggplot2::ggplot(data = mtcars, ggplot2::aes(x = wt, y = mpg, color = gear)) +
#'   ggplot2::geom_point() +
#'   theme_pcj_text()

theme_pcj_text <- function(text, alt_text) {

  if (alt_text == TRUE) {

    labs(
      title = text[1],
      subtitle = text[2],
      x = text[3],
      y = text[4],
      caption = text[5]
    )

  } else {

    labs(
      title = text[1],
      subtitle = NULL,
      x = text[3],
      y = text[4],
      caption = NULL
    )

  }

}
