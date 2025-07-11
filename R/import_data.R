#' File Information Compiler
#'
#' A function to gather information about data files located at `'path'`. Only
#' files of the type specified in `'extension'` will be returned.
#'
#' @param path A filepath string that names a *directory* with files to read.
#' @param extension A string naming the extension of the files to import.
#' @param ... Optional logical arguments of the following to pass to
#' `list.files()`: `'all.files'`, `'recursive'`, `'ignore.case'`,
#' `'include.dirs'`. All default to `FALSE`.
#'
#' @importFrom data.table :=
#'
#' @returns A data.table containing file information from the file(s) found in
#'  the directory specified at `'path'`. Including:
#'  * full filepath
#'  * basename
#'  * extension
#'  * the last date the file was modified
#'  * the age in days since the file was last modified.
#'
#'  This information can be stored in a vector and is intended to be passed to
#'  `import_data()` to read in a list of data files to a single `data.table`
#'  object.
#'
#' @export
#'
#' @examples
#' files_info()

files_info <- function(
    path = system.file("extdata", package = "pcjtools"),
    extension  = "csv",
    ...) {

  if (inherits(x = path, what = "character") &&
        inherits(x = extension, what = "character")) {

    files <- data.table::data.table(
      filepath = list.files(
        path = path,
        pattern = extension,
        full.names = TRUE,
        ...
      )
    )

    files[
      ,
      `:=`(
        basename = basename(files$filepath),
        extension = extension,
        last_mod = file.info(files$filepath)$mtime,
        current_day = file.info(files$filepath)$atime,
        age_days = paste(round(x = abs(file.info(files$filepath)$mtime -
                                         file.info(files$filepath)$atime),
                               digits = 0)
        )
      )
    ]

  } else {
    cat("Error: Wrong object class passed to arguments 'path' or 'extension'.
    Both arguments must be strings of class 'character'.")
  }
}



#' Data Importer
#'
#' A function to import data files from a local directory. Designed
#' to read a list of file path strings, principally, those stored in the
#' first column of the `data.table` produced by the `file_info()` function from
#' `'pcjtools'`.
#'
#' @param x A string or list of strings containing the path of file(s) to
#'  read in; can be the name of a `data.table` column in the form
#'  `dt$filepaths`, which contains filepaths. All files in a list will be read
#'  in and returned as a single `data.table` object.
#'
#' @return a data.table object containing the read-in data from the paths
#' specified in `x`.
#'
#' @export
#'
#' @examples
#' info <- files_info()
#' file <- import_data(x = info$filepath)
#'
#' # alternatively:
#'
#' file <- import_data(x = info[, "filepath"])
#' file <- import_data(x = info[, 1]) # Where '1' is the column with filepath(s)

import_data <- function(x) {

  if (inherits(x = x, what = c("character", "list"))) {

    file_list <- unlist(x)

    read_file_list(files = file_list)

  } else {
    cat("Error: Wrong object class passed to argument 'path'. Object must either
    be a column of a data.table, given in the form 'dt$column' where 'column'
    contains file path strings, or a string of class 'character' containing
    a filepath.")
  }

}
