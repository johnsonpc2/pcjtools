#' Histogram with Optional Normal Curve and Normality Test
#'
#' Plots a density histogram for a numeric vector, data frame, or matrix. When
#'  a data frame or matrix is supplied, all numeric variables are plotted in a
#'  lattice layout. An optional normal curve fitted to the data's mean and
#'  standard deviation can be overlaid, and an optional normality test result
#'  can be annotated in the top-left corner of each plot.
#'
#' @param x A numeric vector, data frame, or matrix. When a data frame or
#'  matrix is supplied, all numeric columns are plotted unless \code{vars} is
#'  specified.
#' @param vars An optional character vector of column names to plot when
#'  \code{x} is a data frame or matrix (e.g., \code{c("intact", "reversed")}).
#'  Non-numeric columns named in \code{vars} are skipped with a warning.
#'  Ignored when \code{x} is a numeric vector.
#' @param breaks The number of breaks for the histogram. Passed to
#'  \code{hist()}. Default is \code{30}.
#' @param normal_curve Logical. Should a normal curve be overlaid on the
#'  histogram? The curve is fitted using the mean and standard deviation of the
#'  data. Default is \code{TRUE}.
#' @param test Controls normality testing. Accepts \code{TRUE}, \code{FALSE},
#'  \code{"s-w"}, \code{"k-s"}, or \code{"a-d"}. \code{TRUE} (default) runs
#'  Shapiro-Wilk, equivalent to \code{"s-w"}. \code{FALSE} skips testing.
#'  \code{"k-s"} runs a Kolmogorov-Smirnov test, using \code{dgof::ks.test()}
#'  when ties are present if the \code{dgof} package is installed.
#'  \code{"a-d"} runs an Anderson-Darling test, which requires the
#'  \code{nortest} package. Test name, statistic, and p-value are annotated in
#'  the top-left corner of each plot.
#' @param ncol The number of columns in the lattice layout when \code{x} is a
#'  data frame or matrix. Default is \code{3}.
#'
#' @returns Produces one or more histogram plots as a side effect. Returns
#'  \code{NULL} invisibly.
#' @export
#'
#' @examples
#' # Single numeric vector
#' fit <- lm(mpg ~ wt, data = mtcars)
#' qq_hist(residuals(fit))
#'
#' # All numeric variables in a data frame
#' qq_hist(mtcars)
#'
#' # Selected variables only, in a 2-column layout
#' qq_hist(mtcars, vars = c("mpg", "wt", "hp", "qsec"), ncol = 2)
#'
#' # Residuals from a multivariate model
#' fit_mv <- lm(cbind(mpg, wt) ~ cyl, data = mtcars)
#' qq_hist(as.data.frame(residuals(fit_mv)))
#'
#' \dontrun{
#' # Anderson-Darling test (requires nortest package)
#' qq_hist(residuals(fit), test = "a-d")
#'
#' # Kolmogorov-Smirnov test (uses dgof::ks.test() if ties present and dgof installed)
#' qq_hist(residuals(fit), test = "k-s")
#' }
qq_hist <- function(x, vars = NULL, breaks = 30, normal_curve = TRUE, test = TRUE,
                    ncol = 3) {

  # ---- internal: single numeric vector plot ----------------------------------

  .plot_one <- function(vec, label) {
    vec <- vec[is.finite(vec)]
    graphics::hist(vec,
                   breaks = breaks,
                   freq   = FALSE,
                   main   = label,
                   xlab   = label,
                   col    = "lightgray",
                   border = "white")

    if (normal_curve) {
      m <- mean(vec)
      s <- stats::sd(vec)
      graphics::curve(stats::dnorm(x, mean = m, sd = s),
                      add = TRUE,
                      col = "steelblue",
                      lwd = 2)
    }

    if (!isFALSE(test)) {
      if (isTRUE(test) || test == "s-w") {
        if (length(vec) > 5000) {
          warning(sprintf("'%s' has n > 5000; Shapiro-Wilk requires n <= 5000. Skipping test.", label))
          return(invisible(NULL))
        }
        result     <- stats::shapiro.test(vec)
        test_name  <- "Shapiro-Wilk"
        stat_label <- "W"
      } else if (test == "k-s") {
        if (any(duplicated(vec))) {
          if (requireNamespace("dgof", quietly = TRUE)) {
            message(sprintf("'%s': ties detected, using dgof::ks.test().", label))
            result <- dgof::ks.test(vec, "pnorm", mean = mean(vec), sd = stats::sd(vec))
          } else {
            warning(sprintf("'%s': ties detected. Install 'dgof' for an exact test. Proceeding with caution.", label))
            suppressWarnings(result <- stats::ks.test(vec, "pnorm", mean = mean(vec), sd = stats::sd(vec)))
          }
        } else {
          result <- stats::ks.test(vec, "pnorm", mean = mean(vec), sd = stats::sd(vec))
        }
        test_name  <- "Kolmogorov-Smirnov"
        stat_label <- "D"
      } else if (test == "a-d") {
        if (!requireNamespace("nortest", quietly = TRUE))
          stop("Package 'nortest' is required for Anderson-Darling. Install with install.packages('nortest').")
        result     <- nortest::ad.test(vec)
        test_name  <- "Anderson-Darling"
        stat_label <- "A"
      } else {
        stop("'test' must be TRUE, FALSE, 's-w', 'k-s', or 'a-d'.")
      }

      graphics::legend("topleft",
                       legend    = c(test_name,
                                     sprintf("%s = %.4f", stat_label, result$statistic),
                                     sprintf("p = %.4f", result$p.value)),
                       bty       = "n",
                       text.font = c(2, 1, 1),
                       cex       = 0.85)
    }
  }

  # ---- dispatch: vector vs data frame / matrix -------------------------------

  if (is.numeric(x)) {

    # Plain numeric vector — original behaviour
    .plot_one(x, deparse(substitute(x)))

  } else if (is.data.frame(x) || is.matrix(x)) {

    df <- as.data.frame(x)

    # Select columns
    if (!is.null(vars)) {
      missing_vars <- setdiff(vars, names(df))
      if (length(missing_vars) > 0)
        stop(sprintf("Variable(s) not found in data: %s", paste(missing_vars, collapse = ", ")))
      df <- df[, vars, drop = FALSE]
    }

    num_cols <- names(df)[sapply(df, is.numeric)]

    if (length(num_cols) == 0)
      stop("No numeric variables found in the supplied data frame.")

    if (!is.null(vars)) {
      non_numeric <- setdiff(vars, num_cols)
      if (length(non_numeric) > 0)
        warning(sprintf("Skipping non-numeric variable(s): %s", paste(non_numeric, collapse = ", ")))
      num_cols <- intersect(vars, num_cols)
    }

    n_plots  <- length(num_cols)
    nrow_val <- ceiling(n_plots / ncol)

    op <- graphics::par(mfrow = c(nrow_val, ncol),
                        mar   = c(3, 3, 2, 1),
                        mgp   = c(1.8, 0.5, 0))
    on.exit(graphics::par(op))

    for (col in num_cols) {
      .plot_one(df[[col]], col)
    }

    # Fill any empty cells in the last row with blank plots
    remainder <- nrow_val * ncol - n_plots
    if (remainder > 0) {
      for (i in seq_len(remainder)) graphics::plot.new()
    }

  } else {
    stop("'x' must be a numeric vector, data frame, or matrix.")
  }
}