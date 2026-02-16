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
