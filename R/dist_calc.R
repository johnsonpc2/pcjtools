#' Calculate Distance Matrices for Integer Columns
#'
#' Extracts integer columns from a data frame or data table and calculates
#' pairwise distance matrices using specified distance metrics. Optionally
#' performs interval analysis (distances based on differences between
#' consecutive values) and converts matrices to long format for easier analysis.
#'
#' @param x A data frame or data table containing at least some integer columns.
#'   Only integer columns will be used for distance calculations.
#' @param distances Character vector specifying which distance metrics to
#'   calculate. Options are "manhattan", "euclidean", and/or "cosine".
#'   Default is all three: \code{c("manhattan", "euclidean", "cosine")}.
#' @param name_key Optional named character vector for mapping row indices to
#'   meaningful stimulus names. Names should be character representations of
#'   row indices (e.g., "1", "2", "3"), and values should be the corresponding
#'   stimulus labels. If provided, distance matrices will be melted to long
#'   format with labeled stimuli. Default is NULL.
#' @param interval_analysis Logical indicating whether to perform interval
#'   analysis. If TRUE, calculates distances based on the differences between
#'   consecutive values within each row (using \code{diff()}). Default is TRUE.
#'
#' @return A named list containing distance matrices and optionally their
#'   long-format versions. The structure depends on the parameters:
#'   \itemize{
#'     \item If \code{name_key = NULL}: Returns distance matrices only
#'     \item If \code{name_key} is provided: Returns both matrices and long-format data tables
#'     \item If \code{interval_analysis = TRUE}: Includes interval-based distances
#'   }
#'
#'   Possible list elements include:
#'   \itemize{
#'     \item \code{Manhattan Matrix}, \code{Euclidean Matrix}, \code{Cosine Matrix}
#'     \item \code{Manhattan Interval Matrix}, \code{Euclidean Interval Matrix}, \code{Cosine Interval Matrix}
#'     \item \code{Manhattan Long}, \code{Euclidean Long}, \code{Cosine Long}
#'     \item \code{Manhattan Interval Long}, \code{Euclidean Interval Long}, \code{Cosine Interval Long}
#'   }
#'
#' @export
#'
#' @examples
#' # Create sample data
#' df <- data.frame(
#'   id = 1:5,
#'   var1 = c(1L, 2L, 3L, 4L, 5L),
#'   var2 = c(2L, 4L, 6L, 8L, 10L),
#'   var3 = c(1L, 3L, 5L, 7L, 9L)
#' )
#'
#' # Basic usage - calculate all distances
#' result <- distance(df)
#'
#' # Calculate only Manhattan and Euclidean distances
#' result <- distance(df, distances = c("manhattan", "euclidean"))
#'
#' # With name mapping for interpretable output
#' name_map <- c("1" = "Stimulus_A", "2" = "Stimulus_B",
#'               "3" = "Stimulus_C", "4" = "Stimulus_D", "5" = "Stimulus_E")
#' result <- distance(df, name_key = name_map)
#'
#' # Access specific results
#' result$`Manhattan Matrix`
#' result$`Euclidean Long`
#'
#' # Without interval analysis
#' result <- distance(df, interval_analysis = FALSE)

distance <- function(x,
                     distances = c("manhattan", "euclidean", "cosine"),
                     name_key = NULL,
                     interval_analysis = TRUE){

  x_name_raw <- deparse(substitute(x))

  # Remove dollar sign and everything before it to shorten variable names
  x_name <- if (grepl("\\$", x_name_raw)) {
    sub(".*\\$", "", x_name_raw)
  } else {
    x_name_raw
  }

  # 1) Take integer columns from X, save them, and turn them into a matrix
  DT = data.table()
  for (i in 1:ncol(x)) {
    if (inherits(x = x[[i]], what = "integer")) {
      DT <- cbind(DT, x[[i]])
    }
  }

  matrix <- as.matrix(DT)

  if (interval_analysis) {
    # Interval Analysis
    interval.matrix <- t(as.matrix(apply(matrix, 1, diff)))
  }

  # 3) Use lapply to calculate all requested distance matrices
  distance_matrices <- lapply(distances, function(method) {
    dist_mat <- calculate_distance(method, matrix)

    # Apply names to matrix rows and columns if name_key is provided
    if (!is.null(name_key)) {
      rownames(dist_mat) <- name_key[rownames(dist_mat)]
      colnames(dist_mat) <- name_key[colnames(dist_mat)]
    }

    return(dist_mat)
  })

  # Name the list elements
  names(distance_matrices) <- distances

  # 3b) If interval_analysis is TRUE, calculate distances on interval matrix
  if (interval_analysis) {
    interval_distance_matrices <- lapply(distances, function(method) {
      dist_mat <- calculate_distance(method, interval.matrix)

      # Apply names to interval matrix rows and columns if name_key is provided
      if (!is.null(name_key)) {
        rownames(dist_mat) <- name_key[rownames(dist_mat)]
        colnames(dist_mat) <- name_key[colnames(dist_mat)]
      }

      return(dist_mat)
    })

    # Name the list elements
    names(interval_distance_matrices) <- distances
  }

  # 4) If name_key is supplied, melt each matrix to long format
  if (!is.null(name_key)) {

    # Use lapply to melt each distance matrix
    dist_long <- lapply(distances, function(method) {
      matrix_melt(
        dist_mat = distance_matrices[[method]],
        name_key = name_key,
        x_name = x_name,
        dist_method = method
      )
    })

    # Name the list elements
    names(dist_long) <- distances

    # Also melt interval matrices if interval_analysis is TRUE
    if (interval_analysis) {
      interval_dist_long <- lapply(distances, function(method) {
        matrix_melt(
          dist_mat = interval_distance_matrices[[method]],
          name_key = name_key,
          x_name = x_name,
          dist_method = method,
          is_interval = TRUE
        )
      })

      # Name the list elements
      names(interval_dist_long) <- distances
    }

  } else {

    # Initialize as NULL when name_key is not provided
    dist_long <- stats::setNames(vector("list", length(distances)), distances)

  }

  # 5) Build the output list - keeping original structure but making it dynamic
  distance.list <- list()

  # Add matrix results based on what was calculated
  if ("manhattan" %in% distances) {
    distance.list[["Manhattan Matrix"]] <- distance_matrices[["manhattan"]]
  }
  if ("euclidean" %in% distances) {
    distance.list[["Euclidean Matrix"]] <- distance_matrices[["euclidean"]]
  }
  if ("cosine" %in% distances) {
    distance.list[["Cosine Matrix"]] <- distance_matrices[["cosine"]]
  }

  # Add interval matrix results if interval_analysis is TRUE
  if (interval_analysis) {
    if ("manhattan" %in% distances) {
      distance.list[["Manhattan Interval Matrix"]] <-
        interval_distance_matrices[["manhattan"]]
    }
    if ("euclidean" %in% distances) {
      distance.list[["Euclidean Interval Matrix"]] <-
        interval_distance_matrices[["euclidean"]]
    }
    if ("cosine" %in% distances) {
      distance.list[["Cosine Interval Matrix"]] <-
        interval_distance_matrices[["cosine"]]
    }
  }

  # Add long format results if they exist
  if (!is.null(name_key)) {
    if ("manhattan" %in% distances) {
      distance.list[["Manhattan Long"]] <- dist_long[["manhattan"]]
    }
    if ("euclidean" %in% distances) {
      distance.list[["Euclidean Long"]] <- dist_long[["euclidean"]]
    }
    if ("cosine" %in% distances) {
      distance.list[["Cosine Long"]] <- dist_long[["cosine"]]
    }

    # Add interval long format results if interval_analysis is TRUE
    if (interval_analysis) {
      if ("manhattan" %in% distances) {
        distance.list[["Manhattan Interval Long"]] <-
          interval_dist_long[["manhattan"]]
      }
      if ("euclidean" %in% distances) {
        distance.list[["Euclidean Interval Long"]] <-
          interval_dist_long[["euclidean"]]
      }
      if ("cosine" %in% distances) {
        distance.list[["Cosine Interval Long"]] <-
          interval_dist_long[["cosine"]]
      }
    }
  }

  # Filter out NULL elements and return
  output <- distance.list[!sapply(distance.list, is.null)]

  return(output)

}
