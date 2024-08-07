#' A function to import jsPsych CSV data files downloaded from cognition.run
#'
#' @param file_dir a string naming a directory filepath that contains
#' files to read.
#' @param file_type a string that gives the name of the
#' file's extension (e.g., "xlsx", "csv", etc.)
#'
#' @return a data.table object containing the read-in data
#' from the specified directory.
#' @export
#' @importFrom fs path_package
#' @importFrom purrr map
#' @importFrom data.table data.table fread rowid setnames tstrsplit `:=`
#' @importFrom magrittr `%>%`
#' @importFrom reshape2 dcast
#'
#' @examples
#' import_csv(file_type = "csv")
import_csv <- function(file_dir = NULL, file_type = NULL) {
  # List Files from Directory -----------------------------------------------

  if (is.null(file_dir)) {
    # Make a list of files to import from the specified directory
    file_list <- dir(system.file("extdata", package = "pcjtools"),
      full.names = TRUE
    )
  } else {
    file_list <- dir(path = file_dir, pattern = file_type, full.names = TRUE)
  }

  # Read In Files -----------------------------------------------------------

  read_data <- function(file_list = "example_data.csv") {
    if (!inherits(file_list,
      what = "character"
    )) {
      stop("Error: file_list must be a character
           vector containinga list of file paths.\n")
    }

    raw_data <- data.table()

    for (filename in file_list) {
      if (!file.exists(filename)) {
        stop("Error: Invalid file path detected in the file list.\n")
      }

      df <- fread(
        file = filename,
        na.strings = c("", "null", NA, "n/a", "na", "n/a.", "[0]"),
        drop = c(
          "success", "internal_node_id", "recorded_at",
          "source_code_version", "ip", "user_agent",
          "browser", "browser_version", "platform",
          "platform_version", "referer", "accept_language",
          "timeout", "failed_images", "failed_audio",
          "failed_video", "view_history", "question_order"
        )
      )

      raw_data <- rbind(raw_data, df)
    }

    return(raw_data)
  }

  raw_data <- read_data(file_list)

  # Add Trial ID Column -----------------------------------------------------

  trial_id <- numeric(0)

  concatenate_columns <- function(data) {
    data[, trial_id := paste0(data$sona_id, "-", data$trial_index)]
    return(data)
  }

  clean_data <- concatenate_columns(raw_data)

  rm(raw_data)

  # Parse String Responses --------------------------------------------------

  distraction_data <- function(data, subject_var) {
    data.table::setkeyv(data, subject_var)

    # make a temporary subset of the data which
    # includes only recorded distraction data rows.
    temp_text <- unique(data[!is.na(data$distraction), "distraction"])

    event_count <- numeric(0)

    for (i in seq_len(nrow(temp_text))) {
      # Focus events tell you when a subject
      # has refocused after becoming distracted.
      event_count[i] <- stringi::stri_count(str = temp_text[i], regex = "focus")
    }

    # Store the event count in a new variable.
    data[!is.na(data$distraction), refocus_events := event_count]

    return(data)
  }

  focus_data <- distraction_data(clean_data, "sona_id")

  return(focus_data)
}
