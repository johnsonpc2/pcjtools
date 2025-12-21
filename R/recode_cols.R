#' An efficient variable recoder to change column types in a datatable
#'
#' @param dt A datatable with columns you wish to recode.
#' @param col_types A named list of columns to recode with how they should be
#'  recoded in the style of col_name = "col_type".
#' @param .verbose Logical. Should results of recoding be reported?
#'
#' @returns Invisibly returns the new datatable with updated columns
#' @export
#'
#' @examples
#' data <- data.table::data.table(
#'   name = rep(c("Alice", "Bob", "Charlie"), length.out = 10),
#'   age = as.character(rep(25:30, length.out = 10)),
#'   score = as.character(runif(10, 70, 100))
#'   )
#'
#' recode_cols(data, list(
#'   age = "integer",
#'   score = "numeric"
#'   ))

recode_cols <- function(dt, col_types, .verbose = FALSE) {
  # Validate inputs
  stopifnot(
    "dt must be a data.table" = data.table::is.data.table(dt),
    "col_types must be a named list" = is.list(col_types) && !is.null(names(col_types))
  )

  # Check which columns exist
  missing_cols <- setdiff(names(col_types), names(dt))
  if (length(missing_cols) > 0) {
    warning("Columns not found in data.table: ", paste(missing_cols, collapse = ", "))
    col_types <- col_types[names(col_types) %in% names(dt)]
  }

  if (length(col_types) == 0) {
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
  for (col in names(col_types)) {
    target_type <- col_types[[col]]

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