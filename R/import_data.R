#' a function to import data files from a local directory
#'
#' @param path a filepath string that names a directory with files to read. All
#' files of the file type named in extension will be read into a single
#' data.table object.
#' @param extension a string naming a kind of file to import.
#' @param ... optional arguments to be passed to list.files
#'
#' @return a data.table object containing the read-in data from the specified
#' directory.
#'
#' @export
#'
#' @examples
#' import_data(extension = "csv")

import_data <- function(path = NULL, extension = NULL, ...) {

  list <- files_info(
    path = path,
    extension = extension
  )

  read_file_list(files = list)

}
