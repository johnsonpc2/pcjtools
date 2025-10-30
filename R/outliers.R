outliers <- function(x, columns, sd_threshold = 2) {

  # Store the smallest outlier value for each column
  smallest_outliers <- numeric(length(columns))
  names(smallest_outliers) <- columns

  # Check each column...
  for (col in seq_along(columns)) {
    col <- columns[col]
    m <- mean(x[[col]], na.rm = TRUE) # Calculate mean
    s <- sd(x[[col]], na.rm = TRUE) # Calculate standard deviations
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