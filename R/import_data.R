#' File Information Compiler
#'
#' A function to gather information about data files of the type specified in
#' the `'extension'` argument and located in the local directory specified in
#' `'path'`.
#'
#' @param path A filepath string that names a directory with files to read.
#' @param extension A string naming the extension of the files to import.
#' @param ... Optional arguments passed to `list.files()`:
#'  `all.files`, `recursive`, `ignore.case`, `include.dirs`, which all default
#'  to FALSE.
#' @importFrom data.table :=
#'
#' @returns A list containing file information from files in the
#'  directory specified in `'path'`. Includes the full filepath, the basename,
#'  the extension, the last date the file was modified, and the age in days
#'  since the file was last modified. This information can be stored in a vector
#'  and is intended to be passed to `import_data()` to read in a list of data
#'  files to a single `data.table` object.
#'
#' @export
#'
#' @examples
#' files_info()

files_info <- function(
    path = system.file("extdata", package = "pcjtools"),
    extension  = "csv",
    ...) {

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
      age_days = paste(round(
        x = abs(file.info(files$filepath)$mtime -
                  file.info(files$filepath)$atime),
        digits = 0)
        )
      )
    ]


}



#' Data Importer
#'
#' A function to import data files from a local directory. Designed
#' specifically to read a list of file path strings, probably stored in the
#' first column of the `data.table` produced by the `file_info()` function.
#'
#' @param x A string or a list of strings containing the path of file(s) to
#'  read in; can be the name of a column, such as `dt$filepaths`, from a
#'  `data.table` object containing filepaths. All files in a list will be read
#'  in and returned as a single `data.table` object.
#'
#' @return a data.table object containing the read-in data from the specified
#' directory.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' import_data(
#' x = )
#' }

import_data <- function(x) {

  if (sum(class(x) %in% c("data.table", "data.frame")) == 2) {

    file_list <- x$filepath

    read_file_list(files = file_list)

  } else {
    cat("Error: Wrong object type passed to argument 'path'. Object must be of
        class 'character'.")
  }

}
