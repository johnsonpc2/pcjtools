#' A suite of functions to customize plot aesthetics
#'
#' @param palette a string, including: "default", "neg_to_pos", "mono_printing",
#' "mono_blue", "mono_red", "mono_yellow", or "ualbany".
#' @param continuous logical. Whether the scale of the data is discrete or continuous.
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
      discrete_scale(palette = colorRampPalette(.palettes[[palette]]),
                     aesthetics = c("color", "fill"),
                     na.value = .colors$na_value)
  } else {
      scale_color_gradientn(colors = .palettes[[palette]],
                            na.value = .colors$na_value)
  }
}


#' A function to download and import the Atkinson Hyperlegible font required for
#' the theme_pcj function
#'
#' @param overwrite if font files are downloaded to the system folder, should
#' existing files be overwritten?
#' @param os_fonts a string, including "all", "pdf", "postscript" and "win",
#' indicating which fonts should be loaded into RStudio
#'
#' @return NULL
#' @export
#' @importFrom usethis use_zip
#' @importFrom extrafont font_import loadfonts
#' @import ragg
#'
#' @examples
#' load_atkinson(overwrite = FALSE, os_fonts = "win")

load_atkinson <- function(overwrite = FALSE, os_fonts = "win") {
  # Check if Atkinson Hyperlegible font files already exist in Windows Font directory
  atkinson_files <- c("AtkinsonHyperlegible-Bold.ttf",
                      "AtkinsonHyperlegible-BoldItalic.ttf",
                      "AtkinsonHyperlegible-Italic.ttf",
                      "AtkinsonHyperlegible-Regular.ttf")
  atkinson_paths <- file.path("C:", "Windows", "Fonts", atkinson_files)

  if (!all(file.exists(atkinson_paths))) {
    # If any of the font files don't exist, download and import them
    use_zip(url = "https://github.com/googlefonts/atkinson-hyperlegible/archive/main.zip",
            destdir = tempdir())

    file.copy(from = c(file.path(paste0(tempdir(), "\\atkinson-hyperlegible-main\\fonts\\ttf\\AtkinsonHyperlegible-Bold.ttf")),
                       file.path(paste0(tempdir(), "\\atkinson-hyperlegible-main\\fonts\\ttf\\AtkinsonHyperlegible-BoldItalic.ttf")),
                       file.path(paste0(tempdir(), "\\atkinson-hyperlegible-main\\fonts\\ttf\\AtkinsonHyperlegible-Italic.ttf")),
                       file.path(paste0(tempdir(), "\\atkinson-hyperlegible-main\\fonts\\ttf\\AtkinsonHyperlegible-Regular.ttf"))),
              to = file.path(paste0("C:\\Windows\\Fonts")),
              overwrite = overwrite)

    font_import(prompt = FALSE)
  }



  loadfonts(device = os_fonts, quiet = T)
}


#' Graph aesthetics to maximize legibility of text on graphs printed for academic posters
#'
#' @param base_size an integer, the size the font in a plot should default to.
#' @param dark_text a quoted hex string, sets the color of the darkest text in a plot.
#'
#' @return a plot configured with aesthetic settings specified by options set by this function
#' @export
#' @importFrom monochromeR generate_palette
#' @importFrom ggplot2 ggplot element_blank element_line element_text labs margin
#' rel theme theme_minimal %+replace%
#'
#' @examples
#' ggplot2::ggplot(data = mtcars, ggplot2::aes(x = wt, y = mpg, color = gear)) +
#' ggplot2::geom_point() +
#' pcj_aesthetics()

pcj_aesthetics <- function(base_size = 12,
                           dark_text = "#000000") {

  mid_text <- generate_palette(colour = dark_text,
                                            modification = "go_lighter",
                                            n_colours = 9)[4]

  light_text <- generate_palette(colour = dark_text,
                                              modification = "go_lighter",
                                              n_colours = 9)[7]

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
        color = mid_text
      ),
      axis.title = element_text(
        size = rel(1.5),
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
      # panel.background = element_blank(),
      # legend.key = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      legend.position = "top",
      legend.text = element_text(
        size = rel(1.5),
        lineheight = 1,
        color = mid_text
      ),
      legend.direction = "horizontal"
      )
}

#' A theme developed to build consistent ggplot graphs
#'
#' @param ggplot.object a ggplot object stored in a vector
#' @param palette a string defining the palette to use, including: "default",
#' "neg_to_pos", "mono_printing", "mono_blue", "mono_red", "mono_yellow", or "ualbany".
#' @param continuous logical. Whether the scale of the data is discrete or continuous.
#' @param base.size an integer, the size the font in a plot should default to.
#' @param dark.text a quoted hex string, sets the color of the darkest text in a plot.
#' @param graph.text a named list of plot labels and the text to fill them.
#'
#' @return a ggplot object following the specified settings
#' @export
#'
#' @examples
#' g1 <- ggplot2::ggplot(data = mtcars,
#' ggplot2::aes(x = mpg, y = wt, color = factor(cyl))) +
#' ggplot2::geom_point()
#' theme_pcj(ggplot.object = g1)

theme_pcj <- function(ggplot.object,
                       palette = "default",
                       continuous = FALSE,
                       base.size = 12,
                       dark.text = "#000000",
                       graph.text = list(title = "title", subtitle = "subtitle", xlab = "xlab", ylab = "ylab", caption = paste("Revised", Sys.time()))) {

  # Subroutine to add a custom palette to a ggplot object
  color_sub <- function(ggplot.object, palette = palette, continuous = continuous) {
    ggplot.object +
      pcj_graph_palettes(
        palette = palette,
        continuous = continuous
      )
  }

  ggplot.object <- color_sub(ggplot.object, palette, continuous)
  # You should add an if statement so the above code will only if "continuous = F"
  # if the variable has been factorized, and if it hasn't, then to factorize it
  # in the function

  # Subroutine to add custom aesthetics to a ggplot object
  aesthetics_sub <- function(ggplot.object, base_size = base.size, dark_text = dark.text) {
    theme.options <- list(options)
    ggplot.object +
      pcj_aesthetics(
        base_size = base.size,
        dark_text = dark.text
      )
  }

  ggplot.object <- aesthetics_sub(ggplot.object)


  # Subroutine to add text labels for plot elements to a ggplot object
  text_sub <- function(ggplot.object, graph.text = list(title = "", subtitle = "", xlab = "", ylab = "", caption = "")) {
    graph.text <- as.list(graph.text)
    ggplot.object +
      labs(
        title = graph.text$title,
        subtitle = graph.text$subtitle,
        x = graph.text$xlab,
        y = graph.text$ylab,
        caption = graph.text$caption
      )
  }

  ggplot.object <- text_sub(ggplot.object, graph.text)

  print(ggplot.object)

}
