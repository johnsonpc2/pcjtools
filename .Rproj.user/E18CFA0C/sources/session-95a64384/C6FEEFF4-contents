#' a function to list information about data files from a local directory
#'
#' @param path a filepath string naming a directory with files to read.
#' @param extension a string naming the extension of the files to import.
#' @param ... optional arguments to be passed to the 'list.files()' function
#' @importFrom data.table :=
#'
#' @returns a data.table object containing file name strings from the specified
#' directory.
#'
#' @export
#'
#' @examples
#' files_info(path = NULL, extension = "csv")

files_info <- function(path = NULL, extension, ...) {

  if (is.null(path)) {

    files <- data.table::data.table(
      file = list.files(
        path = system.file(... = "extdata", package = "pcjtools"),
        full.names = TRUE
      )
    )

  } else {

    files <- data.table::data.table(
      file = list.files(
        path = path,
        pattern = extension,
        full.names = TRUE,
        ...
      )
    )

  }

  files[
    ,
    `:=`(
      basename = basename(file),
      last_mod = file.info(file)[, 4],
      age_days = abs(
        lubridate::day(Sys.Date()) - lubridate::day(file.info(file)[, 4])
      )
    )
  ]

  return(files)

}



#' a helper function used by 'read_file_list()' to read individual data files
#'
#' @param x a string, or list of strings, with file name paths
#'
#' @returns a data.table object containing data from a file supplied to
#' 'read_data_list()'
#'
#' @examples
#' \dontrun{
#' read_file(x)
#' }

read_file <- function(x) {

  data.table::fread(
    file = x,
    na.strings = c("", "null", NA),
    nThread = data.table::getDTthreads(),
    data.table = TRUE
  )

}



#' a function to read in data passed from the 'read_file()' helper function
#'
#' @param files a string or an object containing strings with file path(s)
#'
#' @returns a data.table object of concatenated data from all named files
#' supplied in 'file_list'
#'
#' @export
#'
#' @examples
#' data <- read_file_list(files = files_info())

read_file_list <- function(files) {

  files <- files[, 1]

  if (!inherits(x = files$file, what = "character")) {

    stop("Error: file_list must be an object containing file path strings of
         class 'character'.\n")

  } else {

    list <- purrr::map(
      .x = files,
      .f = read_file
    )

    data.table::rbindlist(list)

  }
}
