#' File Reader
#'
#' A helper used by `'read_file_list()'` to read in individual data files.
#'
#' @param x A filepath in the form of a string.
#'
#' @returns A `data.table` object containing data from a file supplied to
#' `'read_data_list()'`.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' info <- files_info()
#' file <- read_file(x = info$filepath)
#' }

read_file <- function(x) {
  data.table::fread(
    file = x,
    na.strings = c("", "null", NA),
    nThread = data.table::getDTthreads(),
    data.table = TRUE
  )
}



#' A Reader for Lists of Files
#'
#' A wrapper to read in multiple files and bind them together into a single object.
#'
#' @param files A string or an object containing filepath string(s).
#' @param parallel Logical; use parallel processing? Default is FALSE.
#' @param n_cores Integer; number of cores to use. Default uses all but one core.
#' @param show_progress Logical; show progress bar? Default is TRUE.
#' @inheritDotParams data.table::rbindlist
#'
#' @returns A `data.table` object with concatenated data from all named files.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' info <- files_info()
#' # Serial with progress
#' data <- read_file_list(files = info$filepath)
#'
#' # Parallel without progress
#' data <- read_file_list(files = info$filepath, parallel = TRUE)
#' }

read_file_list <- function(files, parallel = FALSE, n_cores = NULL,
                           show_progress = TRUE, ...) {
  # Validate inputs
  if (length(files) == 0) {
    stop("No files provided")
  }

  # Check if files exist
  missing_files <- files[!file.exists(files)]
  if (length(missing_files) > 0) {
    warning("Missing files: ", paste(missing_files, collapse = ", "))
    files <- files[file.exists(files)]
    if (length(files) == 0) stop("No valid files found")
  }

  # Parallel processing with pbapply
  if (parallel && length(files) > 1) {
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }

    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterExport(cl, "read_file", envir = environment())
    parallel::clusterEvalQ(cl, library(data.table))

    dt_list <- pbapply::pblapply(
      files,
      read_file,
      cl = cl
    )

  } else {
    # Serial processing with progress bar
    if (show_progress && length(files) > 1) {
      dt_list <- pbapply::pblapply(files, read_file)
    } else {
      dt_list <- lapply(files, read_file)
    }
  }

  # Efficiently combine all data
  if (show_progress && interactive()) {
    message("\nCombining data...")
  }

  # Set defaults for rbindlist, allow user to override via ...
  rbind_args <- list(
    l = dt_list,
    use.names = TRUE,
    fill = TRUE
  )

  # Merge with user-provided arguments (user args override defaults)
  dots <- list(...)
  rbind_args[names(dots)] <- dots

  do.call(data.table::rbindlist, rbind_args)
}



#' Make Consistently Themed Plots
#'
#' A helper function to set the aesthetics of plots to ensure plots formatted
#' with the `'theme_pcj()'` function all have the same settings.
#'
#' @param base_size The plot's default font size; must be numeric.
#' @param dark_text A quoted hex code that sets the color of the darkest text in
#' the plot. All text is based on shades of the specified hex code.
#' @param font A character string containing the name of the font in which to
#' print the plot's text.
#' @param ... Optional arguments to be passed to `'theme()'`.
#'
#' @import ggplot2
#'
#' @keywords internal
#'
#' @returns A plot configured with the declared aesthetics.

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



#' Plot Color Palettes
#'
#' A helper function defining the palettes accessible to `'theme_pcj()'`.
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
#' @returns A plot object with the specified aesthetics rendered.

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



#' Plot Text Settings
#'
#' A helper to define the text for various elements of a plot as part of the
#' `'theme_pcj()'` function.
#'
#' @param plot_text A named character vector where plot features are names and
#'  the text to be printed in the plot are values (e.g.,
#'  c(title = "Plot Title", etc.)).
#' @param alt_text Logical. Should a subtitle and caption be generated for the
#'  plot?
#'
#' @keywords internal
#'
#' @returns A ggplot object with the text declared in the function call.

theme_pcj_text <- function(plot_text, alt_text) {

  # In case the full expected names of the plot elements aren't given
  names(plot_text) <- match.arg(arg = names(plot_text),
                                choices = c("title", "subtitle",
                                            "xlab", "ylab", "caption"),
                                several.ok = TRUE)

  default_caption <- paste0("Created: ", format(Sys.time(), "%Y%m%d, %H:%M"))

  if (alt_text == TRUE) {

    labs(
      title = if ("title" %in% names(plot_text))
        plot_text[["title"]] else "title",
      subtitle = if ("subtitle" %in% names(plot_text))
        plot_text[["subtitle"]] else NULL,
      x = if ("xlab" %in% names(plot_text))
        plot_text[["xlab"]] else "xlab",
      y = if ("ylab" %in% names(plot_text))
        plot_text[["ylab"]] else "ylab",
      caption = if ("caption" %in% names(plot_text))
        plot_text[["caption"]] else default_caption
    )

  } else {

    labs(
      title = if ("title" %in% names(plot_text))
        plot_text[["title"]] else "title",
      subtitle = NULL,
      x = if ("xlab" %in% names(plot_text))
        plot_text[["xlab"]] else "xlab",
      y = if ("ylab" %in% names(plot_text))
        plot_text[["ylab"]] else "ylab",
      caption = NULL
    )
  }
}
