#' A function to import jsPsych CSV data files downloaded from cognition.run
#'
#' @param file.dir a string. The the filepath of the directory containing the files to be read in.
#' @param file.type a string. The name of a type of file extension (e.g., "xlsx", "csv", etc.)
#'
#' @return a data.table object containing data from files in the specified directory.
#' @export
#' @importFrom fs path_package
#' @importFrom purrr map
#' @importFrom data.table data.table fread
#'
#' @examples
#' import_csv(file.type = "csv")


import_csv <- function(file.dir = NULL, file.type = NULL) {

  if (is.null(file.dir)) {

    # Make a list of files to import from the specified directory
    file.list <-  dir(system.file("extdata", package = "pcjtools"), full.names = T)

  } else {

    file.list <- dir(path = file.dir, pattern = file.type, full.names = T)

  }

  read.data <- function(file.list = "example_data.csv") {

    if (!inherits(file.list, what = "character")) {
      stop("Error: file.list must be a character vector containing a list of file paths.\n")
    }

    raw.data <- data.table()

    for (filename in file.list) {

      if (!file.exists(filename)) {
        stop("Error: Invalid file path detected in the file list.\n")
      }

      df <- fread(file = filename,
                  na.strings = c("", "null", NA, "n/a", "na", "n/a.", "[0]"),
                  drop = c("success", "internal_node_id", "recorded_at",
                           "source_code_version", "ip", "user_agent",
                           "browser", "browser_version", "platform",
                           "platform_version", "referer", "accept_language",
                           "timeout", "failed_images", "failed_audio",
                           "failed_video"))

      raw.data <<- rbind(raw.data, df)
      }
  }

  read.data(file.list)

}
