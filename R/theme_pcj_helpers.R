#' Font Installer
#'
#' A helper function to install the Atkinson Hyperlegible font if it isn't
#' already installed. Atkinson Hyperlegible is the default font used by
#' [theme_pcj()].
#'
#' @param font Provide a string naming the font to install if not already
#'  installed.
#' @returns Invisibly installs fonts.
#' @keywords internal
font_install <- function(font){
  # Check if font is registered
  available_fonts <- systemfonts::system_fonts()$family
  if (font %in% available_fonts) {
    base_family <- font
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
            name = font,
            plain = regular
          )
          base_family <- font
        } else {
          base_family <- "sans"
        }
      }, error = function(e) {
        base_family <- "sans"
      }
      )
    } else {
      base_family <- "sans"
    }
  }
}


#' Plot Color Palettes
#'
#' A helper function defining the palettes accessible to `theme_pcj()`.
#'
#' @param palette A string. The name of the palette to be mapped to a variable,
#' including: "default", "neg_to_pos", "mono_printing", "mono_blue", "mono_red",
#' "mono_yellow", or "ualbany".
#' @param continuous Logical. Is the variable continuous or discrete?
#' @param .colors Loads the colors defined within pcj_colors.
#' @param .palettes Loads the palettes defined within pcj_palettes.
#'
#' @keywords internal
#'
#' @returns A ggplot2 scale object with the specified color palette.
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
      aesthetics = c("color", "fill")
    )
  } else {
    ggplot2::scale_color_gradientn(
      colors = .palettes[[palette]]
    )
  }
}