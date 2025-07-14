#' A Helper to Check if An Object is a ggplot Object
#'
#' @param x An object stored in the environment.
#'
#' @keywords internal
#'
#' @returns A Boolean indicating if an object is a ggplot or not.
#'
#' @examples
#' g1 <- ggplot2::ggplot(
#' data = mtcars,
#' mapping = ggplot2::aes(x = mpg, y = wt, color = factor(cyl))
#' )
#'
#' is.ggplot(x = g1)

is.ggplot <- function(x) {
  inherits(x, c("gg", "ggplot"))
}



#' A Helper to Save Individual Plot Objects
#'
#' @param plot A ggplot object that will be saved to file.
#' @param filename A string providing the file name and path where the plot will
#'  be stored.
#' @param ... Optional arguments to pass to `'ggsave()'`.
#'
#' @keywords internal
#'
#' @returns (Invisibly) saves a ggplot object to file in the directory given in
#'  filename.
#'
#' @examples
#' g1 <- ggplot2::ggplot(
#' data = mtcars,
#' mapping = ggplot2::aes(x = mpg, y = wt, color = factor(cyl))
#' )
#'
#' save_single_plot(plot = g1, filename = "./ExamplePlot.png")

save_single_plot <- function(plot, filename, ...) {

  if (!is_ggplot(plot)) {
    stop("'Plot' must be a ggplot object", call. = FALSE)
  }

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    dpi = 900,
    bg = "transparent",
    ...
  )
}