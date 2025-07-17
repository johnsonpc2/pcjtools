#' File Information Compiler
#'
#' A function to gather information about data files located at `'path'`. Only
#' files of the type specified in `'extension'` will be returned.
#'
#' @param path A filepath string that names a *directory* with files to read.
#' @param extension A string naming the extension of the files to import.
#' @param ... Optional logical arguments to pass to `list.files()`, such as:
#'  `'all.files'`, `'recursive'`, `'ignore.case'`, `'include.dirs'`. All default
#'  to `FALSE`.
#'
#' @importFrom data.table :=
#'
#' @returns A `data.table` containing file information from the file(s) found in
#'  the directory specified at `'path'`. Including the full filepath and the
#'  basename of the file(s).
#'
#'  This information can be stored in a vector and is intended to be passed to
#'  `import_data()` in order to concatenate data files together.
#'
#' @export
#'
#' @examples
#' files_info()

files_info <- function(
    path = system.file("extdata", package = "pcjtools"),
    extension = NULL,
    ...) {

  stopifnot(is.character(path), length(path) == 1L,
            (is.character(extension) || is.null(extension)),
            (length(extension) == 1L) || length(extension) == 0L)

  files <- data.table::data.table(
    filepath = list.files(path = path,
                          pattern = extension,
                          full.names = TRUE,
                          ...)
  )

  files[, basename := basename(path = files$filepath)]

}



#' Data Importer
#'
#' A function to import data files from a local directory. Designed
#' to read a list of file path strings, principally, those stored in the
#' first column of the `data.table` produced by the `file_info()` function from
#' `'pcjtools'`.
#'
#' @param x A string or list of strings—e.g., c(string1, string2)—containing the
#'  path(s) of file(s) to read in, or can be the name of a `data.table` or
#'  `dataframe` column in the form `dt$filepath_col` which contains filepaths.
#'  All files in a list will be read in and returned as a single `data.table`
#'  object.
#'
#' @return A `data.table` object containing the read-in data from the paths
#' specified in `x`.
#'
#' @export
#'
#' @examples
#' info <- files_info()
#' file <- import_data(x = info$filepath)

import_data <- function(x) {

  stopifnot((is.character(x) || is.list(x)), length(x) > 0L)

  read_file_list(files = x)

}
