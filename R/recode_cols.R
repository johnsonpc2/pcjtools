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

  # Ensure supplied columns exist
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
    numeric = as.numeric,
    integer = as.integer,
    character = as.character,
    factor = as.factor,
    logical = as.logical,
    Date = as.Date,
    POSIXct = as.POSIXct,
    IDate = data.table::as.IDate
  )

  # Recode columns
  for (col in names(col_types)) {
    target_type <- col_types[[col]]

    if (.verbose) {
      message("Converting '", col, "' to ", target_type)
    }

    if (target_type %in% names(type_converters)) {
      dt[, (col) := type_converters[[target_type]](get(col))]
    } else {
      # Try custom conversion function
      tryCatch({
        convert_fn <- get(paste0("as.", target_type))
        dt[, (col) := convert_fn(get(col))]
      }, error = function(e) {
        warning("Could not convert column '", col, "' to type '", target_type, "': ", e$message)
      })
    }
  }

  invisible(dt)
}