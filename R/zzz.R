.onLoad <- function(libname, pkgname) {
  # Get path to bundled fonts
  font_dir <- system.file("fonts", package = "pcjtools")

  if (dir.exists(font_dir)) {
    # Find the font files
    font_files <- list.files(font_dir, pattern = "\\.ttf$|\\.otf$",
                             full.names = TRUE, ignore.case = TRUE)

    if (length(font_files) > 0) {
      tryCatch({
        # Identify font file types
        regular <- grep("regular", font_files, ignore.case = TRUE, value = TRUE)[1]
        bold <- grep("bold", font_files, ignore.case = TRUE, value = TRUE)[1]
        bold <- bold[!grepl("italic", bold, ignore.case = TRUE)][1]
        italic <- grep("italic", font_files, ignore.case = TRUE, value = TRUE)[1]
        italic <- italic[!grepl("bold", italic, ignore.case = TRUE)][1]
        bolditalic <- grep("bold.*italic|italic.*bold", font_files,
                           ignore.case = TRUE, value = TRUE)[1]

        # Register with systemfonts
        if (!is.na(regular)) {
          systemfonts::register_font(
            name = "Atkinson Hyperlegible",
            plain = regular,
            bold = if (!is.na(bold)) bold else regular,
            italic = if (!is.na(italic)) italic else regular,
            bolditalic = if (!is.na(bolditalic)) bolditalic else regular
          )
        }
      }, error = function(e) {
        # Silent failure - font will fall back to default
        invisible(NULL)
      })
    }
  }
}
