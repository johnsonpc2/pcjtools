#' Check Statistical Test Assumptions
#'
#' Evaluates the assumptions of a specified statistical test given a data
#'  object or fitted model. Results are printed as a formatted table to the
#'  console and returned invisibly as a data frame. Each assumption is graded
#'  as \code{OK}, \code{WARN}, or \code{FAIL} based on the supplied
#'  \code{alpha}. Assumptions that cannot be evaluated statistically (e.g.,
#'  independence of observations) are flagged as \code{"Verify by design"}.
#'  When normality violations are detected, a sample-size-aware note is
#'  appended to the output.
#'
#' @param test A character string naming the statistical test to check
#'  assumptions for. Case-insensitive. Supported values are:
#'  \itemize{
#'    \item \code{"t-test"} / \code{"ttest"} / \code{"t.test"} —
#'      Independent samples t-test. Requires a numeric vector as \code{data}
#'      and a grouping vector as \code{group}.
#'    \item \code{"paired t-test"} / \code{"paired ttest"} / \code{"paired t.test"} —
#'      Paired samples t-test. Requires \code{group} to identify the two
#'      conditions.
#'    \item \code{"one-sample t-test"} / \code{"one-sample t.test"} —
#'      One-sample t-test. Requires a numeric vector as \code{data}.
#'    \item \code{"anova"} / \code{"one-way anova"} / \code{"aov"} —
#'      One-way ANOVA. Accepts a fitted \code{lm()} or \code{aov()} object,
#'      or a numeric vector with \code{group} supplied. Multivariate responses
#'      (e.g., \code{cbind()} on the left-hand side) are handled by running
#'      checks separately for each response variable.
#'    \item \code{"two-way anova"} — Two-way ANOVA. Requires a fitted model
#'      object.
#'    \item \code{"lm"} / \code{"linear regression"} — Linear regression.
#'      Requires a fitted \code{lm()} object. Checks normality of residuals,
#'      homoscedasticity (Breusch-Pagan, requires \code{lmtest}), independence
#'      of residuals (Durbin-Watson, requires \code{lmtest}), and
#'      multicollinearity (VIF, requires \code{car}) for models with more than
#'      one predictor.
#'    \item \code{"pearson"} / \code{"pearson correlation"} — Pearson
#'      correlation. Requires \code{data} to be a list of two numeric vectors:
#'      \code{list(x, y)}.
#'    \item \code{"spearman"} / \code{"spearman correlation"} — Spearman
#'      correlation. Assumptions are evaluated by design.
#'    \item \code{"mann-whitney"} / \code{"wilcoxon"} / \code{"wilcoxon rank-sum"} —
#'      Mann-Whitney U / Wilcoxon rank-sum test. Requires a numeric vector and
#'      a \code{group} vector with exactly two levels.
#'    \item \code{"paired wilcoxon"} / \code{"wilcoxon signed-rank"} —
#'      Wilcoxon signed-rank test. Evaluates symmetry of pairwise differences
#'      via skewness.
#'    \item \code{"chi-square"} / \code{"chisq"} / \code{"chi-squared"} —
#'      Chi-square test. Requires \code{data} to be a table or matrix of
#'      observed frequencies.
#'    \item \code{"kruskal-wallis"} / \code{"kruskal"} — Kruskal-Wallis test.
#'      Requires a numeric vector and a \code{group} vector.
#'  }
#' @param data The data to check. Accepts a numeric vector, a fitted model
#'  object (\code{lm}, \code{aov}, \code{aovlist}, \code{lme}, or
#'  \code{merMod}), a list of two numeric vectors (Pearson correlation only),
#'  or a table or matrix (chi-square only). Note that \code{anova} table
#'  objects (e.g., from \code{anova()}) are not accepted; supply the fitted
#'  model object instead.
#' @param group Optional. A grouping vector of the same length as \code{data},
#'  or a character string naming the grouping column in a fitted model's model
#'  frame. Required for t-tests, ANOVA (when \code{data} is a raw vector),
#'  Mann-Whitney, Wilcoxon signed-rank, and Kruskal-Wallis.
#' @param alpha Numeric. The significance level used to grade assumption
#'  checks. Results with \code{p < alpha} are graded \code{FAIL}; results with
#'  \code{p} between \code{alpha} and \code{2 * alpha} are graded \code{WARN};
#'  results with \code{p >= 2 * alpha} are graded \code{OK}. Default is
#'  \code{0.05}.
#' @param normality_test The normality test to use internally. One of
#'  \code{"s-w"} (Shapiro-Wilk, default) or \code{"a-d"} (Anderson-Darling,
#'  requires the \code{nortest} package). Shapiro-Wilk automatically falls back
#'  to Anderson-Darling for samples larger than 5000.
#'
#' @returns A data frame with columns \code{Assumption}, \code{Statistic},
#'  \code{P_Value}, and \code{Result}, returned invisibly. The formatted table
#'  and any relevant notes are printed to the console as a side effect.
#' @export
#'
#' @examples
#' # Linear regression with a fitted model object
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' check_assumptions("linear regression", fit)
#'
#' # One-way ANOVA with a fitted model object
#' fit_aov <- aov(mpg ~ cyl, data = mtcars)
#' check_assumptions("anova", fit_aov)
#'
#' # Independent samples t-test with raw vectors
#' check_assumptions("t-test", mtcars$mpg, group = mtcars$am)
#'
#' # Chi-square test with a frequency table
#' check_assumptions("chi-square", table(mtcars$cyl, mtcars$gear))
#'
#' # Capture the results data frame
#' results <- check_assumptions("anova", fit_aov, alpha = 0.01)
#'
#' \dontrun{
#' # Anderson-Darling normality test (requires nortest package)
#' check_assumptions("linear regression", fit, normality_test = "a-d")
#'
#' # Linear regression with Breusch-Pagan and VIF checks (requires lmtest and car)
#' check_assumptions("linear regression", fit)
#' }
check_assumptions <- function(test, data, group = NULL, alpha = 0.05, normality_test = "s-w") {

  # ---- helpers ---------------------------------------------------------------

  .normality <- function(x, label = deparse(substitute(x))) {
    x <- stats::na.omit(x)
    if (normality_test == "s-w") {
      if (length(x) > 5000) {
        warning("Shapiro-Wilk requires n <= 5000. Switching to Anderson-Darling for this variable.")
        return(.normality_ad(x, label))
      }
      r <- stats::shapiro.test(x)
      stat_label <- "W"
    } else if (normality_test == "a-d") {
      return(.normality_ad(x, label))
    } else {
      stop("normality_test must be 's-w' or 'a-d'.")
    }
    list(
      assumption  = paste("Normality:", label),
      statistic   = sprintf("%s = %.4f", stat_label, r$statistic),
      p_value     = r$p.value,
      result      = .grade(r$p.value, alpha)
    )
  }

  .normality_ad <- function(x, label) {
    if (!requireNamespace("nortest", quietly = TRUE))
      stop("Package 'nortest' is required for Anderson-Darling. Install with install.packages('nortest').")
    r <- nortest::ad.test(x)
    list(
      assumption  = paste("Normality:", label),
      statistic   = sprintf("A = %.4f", r$statistic),
      p_value     = r$p.value,
      result      = .grade(r$p.value, alpha)
    )
  }

  .grade <- function(p, a) {
    if      (p >= a * 2)  "OK"
    else if (p >= a)      "WARN"
    else                  "FAIL"
  }

  .row <- function(assumption, statistic = NA, p_value = NA, result) {
    data.frame(
      Assumption = assumption,
      Statistic  = as.character(statistic),
      P_Value    = ifelse(is.na(p_value), NA_real_, round(p_value, 4)),
      Result     = result,
      stringsAsFactors = FALSE
    )
  }

  .check_homogeneity <- function(values, groups) {
    groups <- factor(groups)
    if (nlevels(groups) < 2) return(NULL)
    r <- stats::bartlett.test(values ~ groups)
    list(
      assumption = "Homogeneity of variance (Bartlett)",
      statistic  = sprintf("K2 = %.4f", r$statistic),
      p_value    = r$p.value,
      result     = .grade(r$p.value, alpha)
    )
  }

  # ---- input validation ------------------------------------------------------

  test <- tolower(trimws(test))
  valid_tests <- c(
    "t.test", "t-test", "ttest",
    "paired t.test", "paired t-test", "paired ttest",
    "one-sample t.test", "one-sample t-test",
    "anova", "one-way anova", "aov",
    "two-way anova",
    "lm", "linear regression",
    "pearson", "pearson correlation",
    "spearman", "spearman correlation",
    "mann-whitney", "wilcoxon rank-sum", "wilcoxon",
    "paired wilcoxon", "wilcoxon signed-rank",
    "chi-square", "chisq", "chi-squared",
    "kruskal-wallis", "kruskal"
  )
  if (!test %in% valid_tests)
    stop(sprintf(
      "Unrecognised test: '%s'.\nSupported tests: %s",
      test, paste(unique(valid_tests), collapse = ", ")
    ))

  results <- list()

  # ---- detect model object vs raw data ---------------------------------------

  if (inherits(data, "anova"))
    stop("An 'anova' table object was supplied, but assumption checking requires the fitted model object ",
         "(which contains residuals and the model frame), not the summary table.\n",
         "Please supply the fitted model directly, e.g. your $lm or $aov object.")

  is_model <- inherits(data, c("lm", "aov", "aovlist", "mlm", "lme", "merMod"))

  # ============================================================================
  # t-tests (independent samples)
  # ============================================================================
  if (test %in% c("t.test", "t-test", "ttest")) {

    if (is_model) stop("For t-tests, please supply a numeric vector as 'data' and a grouping vector as 'group'.")
    if (is.null(group)) stop("'group' must be supplied for an independent samples t-test.")

    grps <- split(data, group)
    if (length(grps) != 2) stop("Independent t-test requires exactly 2 groups.")

    results[[1]] <- do.call(.row, .normality(grps[[1]], names(grps)[1]))
    results[[2]] <- do.call(.row, .normality(grps[[2]], names(grps)[2]))
    results[[3]] <- do.call(.row, .check_homogeneity(data, group))
    results[[4]] <- .row("Independence of observations", result = "Verify by design")

    # ============================================================================
    # Paired t-test
    # ============================================================================
  } else if (test %in% c("paired t.test", "paired t-test", "paired ttest")) {

    if (is.null(group)) stop("'group' must be supplied to identify the two paired conditions.")
    grps  <- split(data, group)
    if (length(grps) != 2) stop("Paired t-test requires exactly 2 conditions.")
    diffs <- grps[[1]] - grps[[2]]

    results[[1]] <- do.call(.row, .normality(diffs, "pairwise differences"))
    results[[2]] <- .row("Paired observations (1:1 correspondence)", result = "Verify by design")

    # ============================================================================
    # One-sample t-test
    # ============================================================================
  } else if (test %in% c("one-sample t.test", "one-sample t-test")) {

    if (is_model) stop("Supply a numeric vector for a one-sample t-test.")
    results[[1]] <- do.call(.row, .normality(data, "data"))
    results[[2]] <- .row("Independence of observations", result = "Verify by design")

    # ============================================================================
    # One-way ANOVA
    # ============================================================================
  } else if (test %in% c("anova", "one-way anova", "aov")) {

    if (is_model) {
      res        <- stats::residuals(data)
      mf         <- stats::model.frame(data)
      response   <- mf[[1]]
      grp_col    <- if (!is.null(group)) group else attr(stats::terms(data), "term.labels")[1]
      if (!grp_col %in% names(mf))
        stop(sprintf("Group column '%s' not found in model frame. Available columns: %s",
                     grp_col, paste(names(mf), collapse = ", ")))
      grps_factor <- mf[[grp_col]]
    } else {
      if (is.null(group)) stop("'group' must be supplied for ANOVA when data is a raw vector.")
      if (length(data) != length(group))
        stop("'data' and 'group' must be the same length.")
      res         <- tapply(data, group, function(x) x - mean(x, na.rm = TRUE))
      res         <- unlist(res)
      response    <- data
      grps_factor <- group
    }

    is_multivariate <- is.matrix(response)

    if (is_multivariate) {
      resp_cols <- colnames(response)
      res_mat   <- if (is.matrix(res)) res else matrix(res, ncol = ncol(response))
      for (i in seq_along(resp_cols)) {
        results[[length(results) + 1]] <- do.call(.row, .normality(res_mat[, i], paste("residuals:", resp_cols[i])))
      }
      for (i in seq_along(resp_cols)) {
        hom <- .check_homogeneity(response[, i], grps_factor)
        hom$assumption <- paste("Homogeneity of variance (Bartlett):", resp_cols[i])
        results[[length(results) + 1]] <- do.call(.row, hom)
      }
    } else {
      results[[length(results) + 1]] <- do.call(.row, .normality(res, "residuals"))
      results[[length(results) + 1]] <- do.call(.row, .check_homogeneity(response, grps_factor))
    }
    results[[length(results) + 1]] <- .row("Independence of observations", result = "Verify by design")

    # ============================================================================
    # Two-way ANOVA
    # ============================================================================
  } else if (test == "two-way anova") {

    if (!is_model) stop("Supply a fitted aov() or lm() object for two-way ANOVA.")
    res <- stats::residuals(data)

    results[[1]] <- do.call(.row, .normality(res, "residuals"))
    results[[2]] <- .row("Homogeneity of variance", result = "Verify with Levene/Bartlett per cell")
    results[[3]] <- .row("Independence of observations", result = "Verify by design")
    results[[4]] <- .row("No interaction (if not modelled)", result = "Verify by design")

    # ============================================================================
    # Linear regression
    # ============================================================================
  } else if (test %in% c("lm", "linear regression")) {

    if (!is_model) stop("Supply a fitted lm() object for linear regression.")
    res <- stats::residuals(data)

    results[[1]] <- do.call(.row, .normality(res, "residuals"))

    if (requireNamespace("lmtest", quietly = TRUE)) {
      bp <- lmtest::bptest(data)
      results[[2]] <- .row(
        "Homoscedasticity (Breusch-Pagan)",
        statistic = sprintf("BP = %.4f", bp$statistic),
        p_value   = bp$p.value,
        result    = .grade(bp$p.value, alpha)
      )
      dw <- lmtest::dwtest(data)
      results[[3]] <- .row(
        "Independence of residuals (Durbin-Watson)",
        statistic = sprintf("DW = %.4f", dw$statistic),
        p_value   = dw$p.value,
        result    = .grade(dw$p.value, alpha)
      )
    } else {
      results[[2]] <- .row("Homoscedasticity (Breusch-Pagan)",
                           result = "Install 'lmtest' for automated check: install.packages('lmtest')")
      results[[3]] <- .row("Independence of residuals (Durbin-Watson)",
                           result = "Install 'lmtest' for automated check: install.packages('lmtest')")
    }

    predictors <- attr(stats::terms(data), "term.labels")
    if (length(predictors) > 1) {
      if (requireNamespace("car", quietly = TRUE)) {
        vif_vals   <- car::vif(data)
        max_vif    <- max(vif_vals)
        vif_result <- if (max_vif < 5) "OK" else if (max_vif < 10) "WARN" else "FAIL"
        results[[4]] <- .row(
          "No multicollinearity (VIF)",
          statistic = sprintf("max VIF = %.4f", max_vif),
          result    = vif_result
        )
      } else {
        results[[4]] <- .row("No multicollinearity (VIF)",
                             result = "Install 'car' for automated check: install.packages('car')")
      }
    }

    results[[length(results) + 1]] <- .row("Linearity", result = "Verify visually (residuals vs fitted plot)")

    # ============================================================================
    # Pearson correlation
    # ============================================================================
  } else if (test %in% c("pearson", "pearson correlation")) {

    if (!is.list(data) || length(data) != 2)
      stop("For Pearson correlation, supply data as a list of two numeric vectors: list(x, y).")

    results[[1]] <- do.call(.row, .normality(data[[1]], "variable 1"))
    results[[2]] <- do.call(.row, .normality(data[[2]], "variable 2"))
    results[[3]] <- .row("Linearity", result = "Verify visually")
    results[[4]] <- .row("No significant outliers", result = "Verify visually")

    # ============================================================================
    # Spearman correlation
    # ============================================================================
  } else if (test %in% c("spearman", "spearman correlation")) {

    results[[1]] <- .row("Ordinal or continuous data", result = "Verify by design")
    results[[2]] <- .row("Monotonic relationship", result = "Verify visually")
    results[[3]] <- .row("No significant outliers", result = "Verify visually")

    # ============================================================================
    # Mann-Whitney / Wilcoxon rank-sum
    # ============================================================================
  } else if (test %in% c("mann-whitney", "wilcoxon rank-sum", "wilcoxon")) {

    if (is.null(group)) stop("'group' must be supplied for Mann-Whitney U.")
    grps <- split(data, group)
    if (length(grps) != 2) stop("Mann-Whitney requires exactly 2 groups.")

    hom <- .check_homogeneity(data, group)
    results[[1]] <- .row("Ordinal or continuous data", result = "Verify by design")
    results[[2]] <- do.call(.row, hom)
    results[[3]] <- .row("Independence of groups", result = "Verify by design")

    # ============================================================================
    # Wilcoxon signed-rank (paired)
    # ============================================================================
  } else if (test %in% c("paired wilcoxon", "wilcoxon signed-rank")) {

    if (is.null(group)) stop("'group' must be supplied to identify the two paired conditions.")
    grps  <- split(data, group)
    if (length(grps) != 2) stop("Wilcoxon signed-rank requires exactly 2 conditions.")
    diffs <- grps[[1]] - grps[[2]]

    skew        <- mean((diffs - mean(diffs))^3) / stats::sd(diffs)^3
    skew_result <- if (abs(skew) < 0.5) "OK" else if (abs(skew) < 1) "WARN" else "FAIL"

    results[[1]] <- .row(
      "Symmetry of pairwise differences (skewness)",
      statistic = sprintf("skewness = %.4f", skew),
      result    = skew_result
    )
    results[[2]] <- .row("Paired observations (1:1 correspondence)", result = "Verify by design")

    # ============================================================================
    # Chi-square
    # ============================================================================
  } else if (test %in% c("chi-square", "chisq", "chi-squared")) {

    if (!is.table(data) && !is.matrix(data))
      stop("For chi-square, supply data as a table or matrix of observed frequencies.")

    expected   <- outer(rowSums(data), colSums(data)) / sum(data)
    min_exp    <- min(expected)
    exp_result <- if (min_exp >= 5) "OK" else if (min_exp >= 1) "WARN" else "FAIL"

    results[[1]] <- .row("Categorical data", result = "Verify by design")
    results[[2]] <- .row(
      "Expected cell frequencies >= 5",
      statistic = sprintf("min expected = %.2f", min_exp),
      result    = exp_result
    )
    results[[3]] <- .row("Independence of observations", result = "Verify by design")

    # ============================================================================
    # Kruskal-Wallis
    # ============================================================================
  } else if (test %in% c("kruskal-wallis", "kruskal")) {

    if (is.null(group)) stop("'group' must be supplied for Kruskal-Wallis.")
    hom <- .check_homogeneity(data, group)

    results[[1]] <- .row("Ordinal or continuous data", result = "Verify by design")
    results[[2]] <- do.call(.row, hom)
    results[[3]] <- .row("Independence of observations", result = "Verify by design")
  }

  # ---- assemble and print ----------------------------------------------------

  out <- do.call(rbind, results)
  rownames(out) <- NULL

  # Resolve grade symbols for the return value
  out$Result <- ifelse(out$Result == "OK",   "PASS",
                       ifelse(out$Result == "WARN",  "WARN",
                              ifelse(out$Result == "FAIL",  "FAIL", out$Result)))

  # Use console width, capped at 80, for the separator line
  width <- min(getOption("width", 80L), 80L)
  sep   <- strrep("-", width)

  cat(sprintf("\nAssumption check: %s  (alpha = %s)\n", test, alpha))
  cat(sep, "\n")

  for (i in seq_len(nrow(out))) {
    assumption <- out$Assumption[i]
    statistic  <- out$Statistic[i]
    p_val      <- out$P_Value[i]
    result     <- out$Result[i]

    # Line 1: assumption label
    cat(sprintf("  %s\n", assumption))

    # Line 2: statistic + p-value + result, indented
    stat_str <- if (!is.na(statistic) && statistic != "NA") {
      if (!is.na(p_val)) {
        sprintf("%s,  p = %.4f", statistic, p_val)
      } else {
        statistic
      }
    } else {
      NA_character_
    }

    if (!is.na(stat_str)) {
      cat(sprintf("    %s  |  %s\n\n", stat_str, result))
    } else {
      cat(sprintf("    %s\n\n", result))
    }
  }

  cat(sep, "\n")
  cat("WARN: p between alpha and 2*alpha -- interpret with caution.\n")
  cat("'Verify by design': cannot be tested statistically; requires researcher judgment.\n")

  # Sample size note on normality failures
  n_obs <- if (is_model) stats::nobs(data) else length(data)
  any_normality_fail <- any(grepl("Normality", out$Assumption) &
                              grepl("FAIL|WARN", out$Result))
  if (any_normality_fail) {
    if (n_obs >= 100) {
      cat(sprintf(paste0(
        "\nNOTE (n = %d): Normality violations detected, but your sample size is\n",
        "large. Normality tests are highly sensitive at large n and will flag even\n",
        "trivial departures. By the Central Limit Theorem, the sampling distribution\n",
        "of the mean is likely approximately normal regardless. Consider inspecting\n",
        "residual plots (e.g. qq_hist()) to assess whether violations are meaningful.\n"
      ), n_obs))
    } else if (n_obs >= 30) {
      cat(sprintf(paste0(
        "\nNOTE (n = %d): Normality violations detected. Your sample size is moderate;\n",
        "the Central Limit Theorem offers some protection, but inspect residual plots\n",
        "(e.g. qq_hist()) to determine whether violations are practically meaningful.\n"
      ), n_obs))
    } else {
      cat(sprintf(paste0(
        "\nNOTE (n = %d): Normality violations detected in a small sample. Results\n",
        "may be sensitive to this violation. Consider a non-parametric alternative.\n"
      ), n_obs))
    }
  }
  cat("\n")

  invisible(out)
}