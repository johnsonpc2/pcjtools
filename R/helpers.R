#' File Reader
#'
#' A helper used by [read_file_list()] to read in individual data files.
#'
#' @param x A string giving the file path of a single file to read in.
#'
#' @returns A `data.table` object containing data from a file supplied to
#' `'read_file_list()'`.
#'
#' @keywords internal

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
