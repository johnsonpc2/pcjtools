#' Generate a New R Script from Template
#'
#' Creates a new R script based on a template file, with placeholder text
#' replaced by the provided script name and current date. The template file
#' should be named "script_template.R" and stored in the inst folder of your
#' package.
#'
#' @param script_name A string. The name to give the new script file, without
#'   the .R extension. Defaults to "AnalysisScript", which produces a file
#'   named YYYYMMDD_AnalysisScript.R.
#' @param author A string. The name of the script author. Defaults to
#'   "Pierce C. Johnson".
#' @param dir A string. The path to the directory where the new script will be
#'   saved. Defaults to the current working directory.
#' @param open Logical. If TRUE (default), opens the new script in RStudio
#'   after creation. Only works in interactive sessions.
#'
#' @returns Invisibly returns the path to the newly created script file.
#' @export
#'
#' @examples
#' \donttest{
#' new_script(dir = tempdir(), open = FALSE)
#' new_script(script_name = "my_analysis", dir = tempdir(), open = FALSE)
#' }
new_script <- function(script_name = "AnalysisScript",
                       author = "Pierce C. Johnson",
                       dir = ".",
                       open = TRUE) {

  # Input validation
  stopifnot(
    is.character(script_name) && length(script_name) == 1L,
    is.character(author) && length(author) == 1L,
    is.character(dir) && length(dir) == 1L && dir.exists(dir),
    is.logical(open) && length(open) == 1L
  )

  # Strip .R extension if provided
  script_name <- sub("\\.R$|\\.r$", "", script_name)

  # Get current date
  date <- format(Sys.Date(), "%Y%m%d")

  # Build the output file path
  new_file <- file.path(dir, paste0(date, "_", script_name, ".R"))

  # Check if file already exists to avoid overwriting
  if (file.exists(new_file)) {
    stop(sprintf(
      "File already exists: %s\nChoose a different name or directory.",
      new_file
    ))
  }

  # Locate the template file in the package
  template_path <- system.file("script_template.R", package = "pcjtools")

  if (!file.exists(template_path)) {
    stop("Template file 'script_template.R' not found in pcjtools inst folder.")
  }

  # Read the template
  template_txt <- readLines(template_path, warn = FALSE)

  # Replace placeholders with actual values
  template_txt <- gsub(pattern = "{{script_name}}",
                       replacement = script_name,
                       x = template_txt,
                       fixed = TRUE)

  template_txt <- gsub(pattern = "{{date}}",
                       replacement = date,
                       x = template_txt,
                       fixed = TRUE)

  template_txt <- gsub(pattern = "{{author}}",
                       replacement = author,
                       x = template_txt,
                       fixed = TRUE)

  # Write to new file
  writeLines(template_txt, con = new_file)
  message(sprintf("Created new script: %s", new_file))

  # Open in RStudio if requested and in interactive session
  if (open && interactive()) {
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      rstudioapi::navigateToFile(new_file)
    } else {
      message("RStudio not available. Open the file manually at: ", new_file)
    }
  }

  invisible(new_file)
}