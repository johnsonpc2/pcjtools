#' Write Multiple Data Frames or Data Tables to CSV Files
#'
#' A thin wrapper around \code{\link[data.table]{fwrite}} that iterates over
#' paired lists of data objects and file paths, writing each data.table or
#' data.frame to its corresponding CSV file.
#'
#' @param x A list of \code{data.table} or \code{data.frame} objects to be
#'   written to disk. All elements must be of one of these two types.
#' @param path A list (or character vector) of file path strings, each
#'   specifying the destination for the corresponding element in \code{x}.
#'   Must be the same length as \code{x}.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of writing
#'   files to disk.
#'
#' @details
#' The function validates that \code{x} and \code{path} are the same length
#' before iterating. Each element of \code{x} is checked to be a
#' \code{data.table} or \code{data.frame} before writing; if not, an
#' informative error is thrown indicating which element failed the check.
#' Files are written without row names.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' file_writer(
#'   x = list(df1, df2),
#'   path = list("./output/file1.csv", "./output/file2.csv")
#' )
#' }
file_writer <- function(x, path) {

  stopifnot(length(x) == length(path))

  for (i in seq_along(x)) {

    if (!inherits(x = x[[i]], what = c("data.table", "data.frame"))) {
      stop(
        paste0("Element ", i, " in list x is not a data.table or data.frame object")
      )
    }

    data.table::fwrite(x = x[[i]],
                       file = path[[i]],
                       row.names = FALSE)
  }

    return(invisible(NULL))

}