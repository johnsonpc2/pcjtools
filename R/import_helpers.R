#' File Reader
#'
#' A helper function used by `'read_file_list()'` to read individual data files.
#'
#' @param x A filepath in the form of a string.
#'
#' @returns A `data.table` object containing data from a file supplied to
#' 'read_data_list()'
#'
#' @keywords internal
#'
#' @examples
#' info <- files_info()
#' file <- import_data(x = info$filepath)

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
#' A wrapper to read in multiple files passed from the `'read_file()'` helper
#' function.
#'
#' @param files A string or an object containing filepath string(s).
#'
#' @returns A `data.table` object with concatenated data from all named files
#'  supplied in `'file_list'`
#'
#' @keywords internal
#'
#' @examples
#' info <- files_info()
#' data <- import_data(x = info$filepath)

read_file_list <- function(files) {

  list <- purrr::map(
    .x = files,
    .f = read_file
  )

  data.table::rbindlist(list)

}
