#' Title
#'
#' @param x A dataframe object containing columns and rows of data.
#' @param columns A list of column names outliers will be calculated for.
#' @param sd_threshold The number of standard deviations from the mean an observation must be to be considered an outlier.
#'
#' @returns Returns a list of the outlier values from the specified columns of a dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#' outliers(x = dataframe, columns = c("Var1", "Var2"), sd_threshold = 2)
#' }

outliers <- function(x, columns, sd_threshold = 2) {

  # Store the smallest outlier value for each column
  smallest_outliers <- numeric(length(columns))
  names(smallest_outliers) <- columns

  # Check each column...
  for (col in seq_along(columns)) {
    col <- columns[col]
    m <- mean(x[[col]], na.rm = TRUE) # Calculate mean
    s <- stats::sd(x[[col]], na.rm = TRUE) # Calculate standard deviations
    z_scores <- abs((x[[col]] - m) / s) # Calculate z-scores for observations

    # Identify which values are outliers
    is_outlier <- z_scores > sd_threshold

    # Get the smallest outlier of the outliers in each column
    if (any(is_outlier, na.rm = TRUE)) {
      smallest_outliers[col] <- min(x[[col]][is_outlier], na.rm = TRUE)
    } else {
      smallest_outliers[col] <- NA  # No outliers in this column
    }
  }

  # Return rows with outliers
  return(smallest_outliers)
}