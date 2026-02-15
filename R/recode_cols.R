#' An efficient variable recoder to change column types in a datatable
#'
#' @param dt A datatable with columns you wish to recode.
#' @param cols Either a named list of columns to recode (col_name = "col_type"),
#'   a numeric vector of column indices to recode, or a character vector of column names.
#' @param class Optional. When cols is a numeric/character vector, this specifies
#'   the target type for all columns. Can be a single type (applied to all) or a vector
#'   matching the length of cols. Ignored when cols is a named list.
#' @param .verbose Logical. Should results of recoding be reported?
#'
#' @returns Invisibly returns the new datatable with updated columns
#' @export
#'
#' @examples
#' data <- data.table::data.table(
#'   name = rep(c("Alice", "Bob", "Charlie"), length.out = 10),
#'   age = as.character(rep(25:30, length.out = 10)),
#'   score = as.character(runif(10, 70, 100)),
#'   height = as.character(runif(10, 150, 190))
#' )
#'
#' # Method 1: Named list (original functionality)
#' recode_cols(data, list(
#'   age = "integer",
#'   score = "numeric"
#' ))
#'
#' # Method 2: Column indices with single type
#' recode_cols(data, cols = 2:4, class = "numeric")
#'
#' # Method 3: Column indices with vector of types
#' recode_cols(data, cols = 2:4, class = c("integer", "numeric", "numeric"))
#'
#' # Method 4: Column names with single type
#' recode_cols(data, cols = c("age", "score"), class = "numeric")
#'
#' # Method 5: Column names with vector of types
#' recode_cols(data, cols = c("age", "score"), class = c("integer", "numeric"))
recode_cols <- function(dt, cols, class = NULL, .verbose = FALSE) {

  # Validate inputs
  stopifnot("dt must be a data.table" = data.table::is.data.table(dt))

  # Convert cols to named list format
  if (is.list(cols) && !is.null(names(cols))) {
    # Original named list format - use as is
    if (!is.null(class)) {
      warning("class is ignored when cols is a named list")
    }
    col_types_list <- cols

  } else if (is.numeric(cols) || is.character(cols)) {
    # Numeric indices or character column names

    if (is.null(class)) {
      stop("class must be specified when cols is a numeric or character vector")
    }

    # Convert numeric indices to column names
    if (is.numeric(cols)) {
      if (any(cols < 1 | cols > ncol(dt))) {
        stop("Column indices must be between 1 and ", ncol(dt))
      }
      col_names <- names(dt)[cols]
    } else {
      col_names <- cols
    }

    # Handle class
    if (length(class) == 1) {
      # Single type applied to all columns
      col_types_list <- stats::setNames(
        as.list(rep(class, length(col_names))),
        col_names
      )
    } else if (length(class) == length(col_names)) {
      # Vector of types matching column count
      col_types_list <- stats::setNames(as.list(class), col_names)
    } else {
      stop("class must be either length 1 or match the length of cols (",
           length(col_names), ")")
    }

  } else {
    stop("cols must be either a named list, numeric vector, or character vector")
  }

  # Check which columns exist
  missing_cols <- setdiff(names(col_types_list), names(dt))
  if (length(missing_cols) > 0) {
    warning("Columns not found in data.table: ", paste(missing_cols, collapse = ", "))
    col_types_list <- col_types_list[names(col_types_list) %in% names(dt)]
  }

  if (length(col_types_list) == 0) {
    warning("No valid columns to recode")
    return(invisible(dt))
  }

  # Type conversion mapping
  type_converters <- list(
    numeric = function(x) suppressWarnings(as.numeric(x)),
    integer = function(x) suppressWarnings(as.integer(x)),
    character = as.character,
    factor = as.factor,
    logical = as.logical,
    Date = function(x) suppressWarnings(as.Date(x)),
    POSIXct = function(x) suppressWarnings(as.POSIXct(x)),
    IDate = function(x) suppressWarnings(data.table::as.IDate(x))
  )

  # Recode columns
  for (col in names(col_types_list)) {
    target_type <- col_types_list[[col]]

    if (.verbose) {
      na_count <- sum(is.na(dt[[col]]))
      message("Converting '", col, "' to ", target_type,
              " (", na_count, " NAs present)")
    }

    if (target_type %in% names(type_converters)) {
      dt[, (col) := type_converters[[target_type]](get(col))]
    } else {
      # Try custom conversion function
      tryCatch({
        convert_fn <- get(paste0("as.", target_type))
        dt[, (col) := suppressWarnings(convert_fn(get(col)))]
      }, error = function(e) {
        warning("Could not convert column '", col, "' to type '", target_type, "': ", e$message)
      })
    }

    # Warn if conversion introduced new NAs
    if (.verbose) {
      new_na_count <- sum(is.na(dt[[col]]))
      if (new_na_count > na_count) {
        message("  Warning: ", new_na_count - na_count,
                " additional NAs introduced during conversion")
      }
    }
  }

  invisible(dt)
}