#' Import CSV file data downloaded from jsPsych and Cognition.run. This function
#' assumes data files are stored in a folder named "Data" in the working directory
#'
#' @param select A vector of column names or numbers to keep, drop the rest.
#' Select may specify types too in the same way as colClasses; i.e., a vector of
#' colname=type pairs, or a list of type=col(s) pairs. In all forms of select,
#' the order that the columns are specified determines the order of the columns
#' in the result.
#'
#' @return A data frame, a matrix-like structure whose columns may be of
#' differing types (numeric, logical, factor and character and so on).
#' @export
#'
#' @examples
#' csv.cognition.import(select = c(phase = 'character', trial_type = "character",
#' trial_index = "numeric", time_elapsed = "numeric", run_id = "numeric",
#' condition = "factor", recorded_at = "character", rt = "numeric",
#' stimulus = "character", response = "character"))

csv.cognition.import <- function(select = tidyselect::everything()) {
  list.files(
    path = "./Data",
    pattern = ".csv",
    full.names = T
  ) |>

    purrr::map_df(
      ~data.table::fread(
        file = .,
        na.strings = c("", "null", NA, "n/a", "na", "n/a."),
        select = NULL,
        drop = c("success", "timeout", "failed_images", "failed_audio",
                 "failed_video", "internal_node_id", "source_code_version",
                 "user_agent", "browser", "browser_version", "platform",
                 "platform_version", "referer", "accept_language", "view_history",
                 "ip"),
        colClasses = list(
          character = "trial_type", numeric = "trial_index", numeric = "time_elapsed",
          numeric = "run_id", numeric = "condition", character = "recorded_at"
        )
      )
    ) |>

    as.data.frame()
}