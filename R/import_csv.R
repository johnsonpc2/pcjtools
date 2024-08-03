#' A function to import jsPsych CSV data files downloaded from cognition.run
#'
#' @param file.dir a string. The the filepath of the directory containing the files to be read in.
#' @param file.type a string. The name of a type of file extension (e.g., "xlsx", "csv", etc.)
#'
#' @return a data.table object containing data from files in the specified directory.
#' @export
#' @importFrom fs path_package
#' @importFrom purrr map
#' @importFrom data.table data.table fread `:=`
#'
#' @examples
#' import_csv(file.type = "csv")


import_csv <- function(file.dir = NULL, file.type = NULL) {

# List Files from Directory -----------------------------------------------

  if (is.null(file.dir)) {

    # Make a list of files to import from the specified directory
    file.list <-  dir(system.file("extdata", package = "pcjtools"), full.names = T)

  } else {

    file.list <- dir(path = file.dir, pattern = file.type, full.names = T)

  }

# Read In Files -----------------------------------------------------------

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
                           "failed_video", "view_history"))

      raw.data <- rbind(raw.data, df)
    }

    return(raw.data)
  }

  raw.data <- read.data(file.list)

# Add Trial ID Column -----------------------------------------------------

  trial_id <- numeric(0)

  concatenate_columns <- function(data) {

    data[, trial_id := paste0(data$sona_id, "-", data$trial_index)]
    return(data)
  }

  clean.data <- concatenate_columns(raw.data)

  rm(raw.data)

  return(clean.data)
}
