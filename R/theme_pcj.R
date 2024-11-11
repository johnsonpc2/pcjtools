#' A collection of color palettes to be used in the generation of ggplot graphs.
#' This function is wrapped in theme_pcj and cannot be used directly.
#'
#' @param palette a string, including: "default", "neg_to_pos", "mono_printing",
#' "mono_blue", "mono_red", "mono_yellow", or "ualbany".
#' @param continuous logical. Whether the palette used in the graph should use a
#' discrete or continuous scale.
#' @param .colors loads colors defined in pcj_colors.
#' @param .palettes loads palettes defined in pcj_palettes.
#'
#' @return a plot object with the specified aesthetics rendered.
#' @export
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' pcj_graph_palettes(palette = "default")
#' pcj_graph_palettes(palette = "mono_printing")
pcj_graph_palettes <- function(palette = "default",
                               continuous = FALSE,
                               .colors = pcj_colors,
                               .palettes = pcj_palettes) {
  # define colors
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

  # Combine your colors into different palettes
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
    discrete_scale(
      palette = colorRampPalette(.palettes[[palette]]),
      aesthetics = c("color", "fill"),
      na.value = .colors$na_value
    )
  } else {
    scale_color_gradientn(
      colors = .palettes[[palette]],
      na.value = .colors$na_value
    )
  }
}


#' A function to install the Atkinson Hyperlegible font required for the
#' theme_pcj
#'
#'
#' @return NULL
#' @export
#' @importFrom extrafont font_import loadfonts ttf_import
#' @import ragg
#'
#' @examples
#' install_atkinson()

install_atkinson <- function() {

  path <- system.file("fonts", package = "pcjtools")

  ttf_import(paths = path,
             recursive = FALSE,
             pattern = "Atkinson")

  loadfonts(quiet = TRUE)
}


#' Aesthetics to maximize legibility of graphs printed for academic posters.
#' This function is wrapped in theme_pcj and cannot be used directly.
#'
#' @param base_size an integer. The graph's default font size.
#' @param dark_text a quoted hex string, sets the color of the darkest text in a
#' plot. All text is based on various shades of the specified hex code.
#'
#' @return a plot configured with aesthetic settings specified by options set by
#' this function
#' @export
#' @importFrom monochromeR generate_palette
#' @importFrom ggplot2 ggplot element_blank element_line element_text labs
#' margin rel theme theme_minimal %+replace%
#'
#' @examples
#' ggplot2::ggplot(data = mtcars, ggplot2::aes(x = wt, y = mpg, color = gear)) +
#'   ggplot2::geom_point() +
#'   pcj_aesthetics()
pcj_aesthetics <- function(base_size = 12,
                           dark_text = "#000000") {
  mid_text <- generate_palette(
    colour = dark_text,
    modification = "go_lighter",
    n_colours = 9
  )[4]

  light_text <- generate_palette(
    colour = dark_text,
    modification = "go_lighter",
    n_colours = 9
  )[7]

  theme_minimal() %+replace%
    theme(
      text = element_text(
        # family = "Atkinson Hyperlegible",
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
      legend.direction = "horizontal"
    )
}

#' A theme developed to build consistent ggplot graphs
#'
#' @param ggplot_object a ggplot object stored in a vector.
#' @inheritParams pcj_graph_palettes
#' @inheritParams pcj_aesthetics
#' @param graph_text a named list of plot labels and the text to fill them.
#'
#' @return a ggplot object following the specified settings
#' @export
#'
#' @examples
#' g1 <- ggplot2::ggplot(
#'   data = mtcars,
#'   ggplot2::aes(x = mpg, y = wt, color = factor(cyl))
#' ) +
#'   ggplot2::geom_point()
#' theme_pcj(ggplot_object = g1)
theme_pcj <- function(ggplot_object,
                      palette = "default",
                      continuous = FALSE,
                      base_size = 12,
                      dark_text = "#000000",
                      graph_text =
                        list(
                          title = "title",
                          subtitle = "subtitle",
                          xlab = "xlab",
                          ylab = "ylab",
                          caption = paste("Revised", Sys.time())
                        )) {
  # Color Subroutine --------------------------------------------------------
  color_sub <- function(ggplot_object,
                        palette = palette,
                        continuous = continuous) {

    ggplot_object +
      pcj_graph_palettes(
        palette = palette,
        continuous = continuous
      )
  }

  ggplot_object <- color_sub(ggplot_object, palette, continuous)
  # You should add an if statement so the above code will only if
  # "continuous = F" if the variable has been factorized, and if it hasn't,
  # then to factorize it in the function


  # Custom Aesthetics Subroutine --------------------------------------------
  aesthetics_sub <- function(ggplot_object,
                             base_text_size = base_size,
                             text_color = dark_text) {
    ggplot_object +
      pcj_aesthetics(
        base_size = base_text_size,
        dark_text = text_color
      )
  }

  ggplot_object <- aesthetics_sub(ggplot_object)


  # Graph Labels Subroutine -------------------------------------------------
  text_sub <- function(ggplot_object,
                       graph_text = list(
                         title = "",
                         subtitle = "",
                         xlab = "",
                         ylab = "",
                         caption = ""
                       )) {
    graph_text <- as.list(graph_text)
    ggplot_object +
      labs(
        title = graph_text$title,
        subtitle = graph_text$subtitle,
        x = graph_text$xlab,
        y = graph_text$ylab,
        caption = graph_text$caption
      )
  }

  ggplot_object <- text_sub(ggplot_object, graph_text)

  print(ggplot_object) # tes
}
