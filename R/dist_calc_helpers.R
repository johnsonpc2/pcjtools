#' Calculate Distance Between Rows of a Matrix
#'
#' Calculates pairwise distances between all rows in a matrix using the
#' specified distance metric. For cosine distance, implements a custom
#' calculation. For Manhattan and Euclidean distances, uses the built-in
#' \code{\link[stats]{dist}} function.
#'
#' @param method Character string specifying the distance metric. One of
#'   "manhattan", "euclidean", or "cosine".
#' @param matrix Numeric matrix where each row represents a point and distances
#'   will be calculated between all pairs of rows.
#'
#' @return A symmetric matrix of pairwise distances with dimensions equal to
#'   the number of rows in the input matrix. Row and column names are set to
#'   row indices (1, 2, 3, ...).
#'
#' @keywords internal

calculate_distance <- function(method, matrix) {

  stopifnot(is.matrix(matrix), method %in% c("cosine", "manhattan", "euclidean"))

  if (method == "cosine") {

    # Calculate cosine similarity
    cos.sim <- matrix(
      data = 0,
      nrow = nrow(matrix),
      ncol = nrow(matrix),
      dimnames = list(1:nrow(matrix), 1:nrow(matrix))
    )

    for (i in 1:nrow(cos.sim)) {
      for (j in 1:nrow(cos.sim)) {
        cos.sim[i, j] <-
          sum(matrix[i,] * matrix[j,])/sqrt(sum(matrix[i,]^2) *
                                              sum(matrix[j,]^2))
      }
    }

    return(cos.sim)

  } else {

    # Use built-in dist() function for Manhattan and euclidean
    return(as.matrix(stats::dist(x = matrix, method = method)))

  }
}



#' Convert Distance Matrix to Long Format
#'
#' Melts a distance matrix into long format and maps numeric indices to
#' meaningful stimulus names using a provided key.
#'
#' @param dist_mat A square distance matrix, typically output from
#'   \code{calculate_distance()}.
#' @param name_key A named character vector where names are numeric indices
#'   (as characters) and values are stimulus names. Used to map matrix
#'   row/column indices to meaningful labels.
#' @param x_name Character string used to construct the column name for
#'   distance values in the output (typically the name of the original data).
#' @param dist_method Character string indicating the distance method used
#'   (e.g., "manhattan", "euclidean", "cosine"). Used to construct the column
#'   name for distance values.
#' @param is_interval Logical indicating whether the distances were calculated
#'   on interval data (differences between consecutive values). If TRUE, adds
#'   "_interval" suffix to the distance value column name. Default is FALSE.
#'
#' @return A data.table in long format with three columns:
#'   \itemize{
#'     \item \code{stimulus}: The stimulus name for the current row
#'     \item \code{prev_stimulus}: The stimulus name for comparison
#'     \item \code{<x_name>_<dist_method>[_interval]}: The distance value
#'   }
#'
#' @keywords internal

matrix_melt <- function(dist_mat, name_key, x_name,
                        dist_method, is_interval = FALSE){
  suffix <- if (is_interval) "_interval" else ""
  value_name <- paste0(x_name, "_", dist_method, suffix)

  long.temp <- data.table::melt(
    data = data.table::as.data.table(dist_mat, keep.rownames = "stimulus"),
    id.var = 'stimulus',
    variable.name = 'prev_stimulus',
    value.name = value_name
  )

  return(long.temp)
}