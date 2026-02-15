#' Parse and Reshape JSON-Style Survey Responses to Wide Format
#'
#' Extracts key-value pairs from JSON-style response strings and reshapes them
#' into wide format with one row per subject. When a subject has multiple
#' response rows, all responses are combined into a single row. Duplicate
#' question keys are handled by appending numeric suffixes (_2, _3, etc.).
#'
#' @param DT A data frame or data.table containing survey response data. Must
#'   include a column with subject identifiers and a column named "response"
#'   containing JSON-style strings with question-answer pairs in the format:
#'   \code{'{"question1":"answer1","question2":"answer2"}'} or
#'   \code{'{"question1":1,"question2":2}'} (with unquoted numeric values).
#' @param id_col Character string specifying the name of the column containing
#'   subject identifiers. Default is "sona_id".
#' @param prefix Optional character string specifying the name of a column
#'   to use as a prefix for question names. If provided, each question key will
#'   be prefixed with the value from this column (e.g., "phase_questionkey").
#'   Default is NULL (no prefix).
#'
#' @return A data.table in wide format with one row per unique subject. The
#'   first column is the subject identifier, followed by one column for each
#'   unique question key found in the responses. If a subject has the same
#'   question answered multiple times (e.g., across different rows), duplicate
#'   keys are renamed with numeric suffixes (question_2, question_3, etc.).
#'   Missing responses are filled with NA.
#'
#' @export
#'
#' @examples
#' # Create sample survey data with JSON-style responses (quoted values)
#' survey_data <- data.table::data.table(
#'   sona_id = c(101, 101, 102, 103),
#'   response = c(
#'     '{"musicalIdentity":"No","musicalExperience":"I do not sing"}',
#'     '{"audioExperience":"5","normalHearing":"Yes"}',
#'     '{"musicalIdentity":"Yes","musicalExperience":"I play piano"}',
#'     '{"musicalIdentity":"No","hearingAbility":"Excellent"}'
#'   )
#' )
#'
#' # Parse responses to wide format
#' result <- widen_responses(survey_data, id_col = "sona_id")
#'
#' # Create sample survey data with numeric values (unquoted)
#' survey_data2 <- data.table::data.table(
#'   sona_id = c(201, 202),
#'   response = c(
#'     '{"Automatic":4,"Names":4,"Driving":5}',
#'     '{"Automatic":3,"Names":2,"Driving":4}'
#'   )
#' )
#' result2 <- widen_responses(survey_data2, id_col = "sona_id")
#'
#' # With prefix column (e.g., survey phase)
#' survey_data3 <- data.table::data.table(
#'   sona_id = c(301, 301, 302),
#'   phase = c("mindfulness_survey", "satisfaction_survey", "mindfulness_survey"),
#'   response = c(
#'     '{"Automatic":4,"Names":4}',
#'     '{"Important":2,"Ideal":1}',
#'     '{"Automatic":3,"Names":2}'
#'   )
#' )
#' result3 <- widen_responses(survey_data3, id_col = "sona_id", prefix = "phase")
#' # Result will have columns like: mindfulness_survey_Automatic, satisfaction_survey_Important
#'
#' # With custom ID column name
#' survey_data4 <- data.table::data.table(
#'   participant_id = c(1, 2),
#'   response = c(
#'     '{"question1":"answer1","question2":"answer2"}',
#'     '{"question1":"answer3","question3":"answer4"}'
#'   )
#' )
#' result4 <- widen_responses(survey_data4, id_col = "participant_id")

widen_responses <- function(DT, id_col = "sona_id", prefix = NULL) {

  # Convert to data.table if not already
  if (!inherits(x = DT, what = "data.table")) {
    DT <- data.table::as.data.table(DT)
  }

  # Make a copy to avoid modifying by reference
  DT <- data.table::copy(DT)

  # Extract all key-value pairs from all rows for each subject
  result_list <- lapply(unique(DT[[id_col]]), function(subj_id) {

    # Get all responses for this subject
    subj_responses <- DT[get(id_col) == subj_id, response]

    # Get prefix values if prefix is specified
    if (!is.null(prefix)) {
      subj_prefixes <- DT[get(id_col) == subj_id, get(prefix)]
    }

    # Combine all responses into one list
    all_values <- list()

    for (i in seq_along(subj_responses)) {
      response_str <- subj_responses[i]

      # Get the prefix for this response if prefix is specified
      current_prefix <- if (!is.null(prefix)) {
        paste0(subj_prefixes[i], "_")
      } else {
        ""
      }

      # Pattern 1: Match "key":"value" (quoted strings)
      matches_quoted <- stringr::str_match_all(response_str, '\\""([^""]+)"":\\s*\\""([^""]+)\\""')[[1]]

      if (nrow(matches_quoted) > 0) {
        for (j in 1:nrow(matches_quoted)) {
          key <- paste0(current_prefix, matches_quoted[j, 2])
          value <- matches_quoted[j, 3]

          # If key already exists, append a number to make it unique
          if (key %in% names(all_values)) {
            counter <- 2
            new_key <- paste0(key, "_", counter)
            while (new_key %in% names(all_values)) {
              counter <- counter + 1
              new_key <- paste0(key, "_", counter)
            }
            all_values[[new_key]] <- value
          } else {
            all_values[[key]] <- value
          }
        }
      }

      # Pattern 2: Match "key":["value"] (arrays with quoted values)
      array_matches <- stringr::str_match_all(response_str, '\\""([^""]+)"":\\s*\\[\\""([^""]+)\\""\\]')[[1]]

      if (nrow(array_matches) > 0) {
        for (j in 1:nrow(array_matches)) {
          key <- paste0(current_prefix, array_matches[j, 2])
          value <- array_matches[j, 3]

          # If key already exists, append a number to make it unique
          if (key %in% names(all_values)) {
            counter <- 2
            new_key <- paste0(key, "_", counter)
            while (new_key %in% names(all_values)) {
              counter <- counter + 1
              new_key <- paste0(key, "_", counter)
            }
            all_values[[new_key]] <- value
          } else {
            all_values[[key]] <- value
          }
        }
      }

      # Pattern 3: Match "key":number (unquoted numeric values or empty strings)
      # This pattern should NOT match if the value starts with a quote
      matches_numeric <- stringr::str_match_all(response_str, '\\""([^""]+)"":\\s*([^""\\[,}]+)(?=[,}])')[[1]]

      if (nrow(matches_numeric) > 0) {
        for (j in 1:nrow(matches_numeric)) {
          key <- paste0(current_prefix, matches_numeric[j, 2])
          value <- trimws(matches_numeric[j, 3])

          # Only add if this key hasn't been added by previous patterns
          if (!(key %in% names(all_values))) {
            all_values[[key]] <- value
          }
        }
      }
    }

    # Convert to data.table with one row
    if (length(all_values) > 0) {
      dt_row <- data.table::as.data.table(all_values)
      dt_row[, (id_col) := subj_id]
      return(dt_row)
    } else {
      return(NULL)
    }
  })

  # Combine all rows
  result_dt <- data.table::rbindlist(result_list, fill = TRUE)

  # Put ID column first - only if it exists in the result
  if (id_col %in% names(result_dt)) {
    other_cols <- setdiff(names(result_dt), id_col)
    data.table::setcolorder(result_dt, c(id_col, other_cols))
  }

  return(result_dt)
}