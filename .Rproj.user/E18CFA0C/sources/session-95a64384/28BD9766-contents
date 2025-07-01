.onLoad <- function(libname, pkgname) {

  # Register Atkinson Hyperlegible font with systemfonts
  font_path <- system.file("fonts", package = "pcjtools")

  if (dir.exists(font_path) && length(list.files(font_path, pattern = "\\.ttf$|\\.otf$")) > 0) {
    try({
      systemfonts::register_font(
        name = "Atkinson Hyperlegible",
        plain = file.path(font_path, "AtkinsonHyperlegible-Regular.ttf"),
        bold = file.path(font_path, "AtkinsonHyperlegible-Bold.ttf"),
        italic = file.path(font_path, "AtkinsonHyperlegible-Italic.ttf"),
        bolditalic = file.path(font_path, "AtkinsonHyperlegible-BoldItalic.ttf")
      )
    }, silent = TRUE)
  }
}