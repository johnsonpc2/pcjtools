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