#' a helper function used by 'read_file_list()' to read individual data files
#'
#' @param x a string with file name paths
#'
#' @returns a data.table object containing data from a file supplied to
#' 'read_data_list()'
#'
#' @keywords internal
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
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' data <- read_file_list(files = files_info())
#' }

read_file_list <- function(files) {

    list <- purrr::map(
      .x = files,
      .f = read_file
    )

    data.table::rbindlist(list)

}
