#' Split a string
#'
#' @param x A character vector with one element.
#' @param split The delimiter in x to split by.
#'
#' @return A character vector whose length will be the number of words from
#' the delimited work list in vector x.
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' strsplit1(x, split = ",")

strsplit1 <- function(x, split) {
  strsplit(x, split = split)[[1]]
}