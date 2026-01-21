#' Negative Log Likelihood for Diffusion Model
#'
#' Computes the negative log likelihood of diffusion model parameters given
#' observed response time and choice data. Used as the objective function for
#' maximum likelihood parameter estimation (Ratcliff & Tuerlinckx, 2002).
#'
#' @param par Named numeric vector of parameters in the format `param[i]` where
#'   param is the parameter name (a, v, w, t0, sv, sw, st0) and i is the index.
#' @param rt Numeric vector of response times in seconds.
#' @param response Numeric vector of responses (typically 1 or 2, representing
#'   lower or upper boundary).
#' @param bound_index Integer vector mapping each trial to its boundary
#'   separation (a) and starting point (w) parameter index.
#' @param drift_index Integer vector mapping each trial to its drift rate (v)
#'   parameter index.
#' @param resid_index Integer vector mapping each trial to its nondecision time
#'   (t0) parameter index.
#' @param sv_index Integer vector mapping each trial to its drift rate
#'   variability (sv) parameter index. Can be NULL if not estimated.
#' @param sw_index Integer vector mapping each trial to its starting point
#'   variability (sw) parameter index. Can be NULL if not estimated.
#' @param st0_index Integer vector mapping each trial to its nondecision time
#'   variability (st0) parameter index. Can be NULL if not estimated.
#' @param ... Additional arguments passed to [WienR::WienerPDF()].
#'
#' @returns Negative log likelihood value. Returns `Inf` if PDF evaluation fails.
#' @references
#' Ratcliff, R., & Tuerlinckx, F. (2002). Estimating parameters of the diffusion
#'  model: Approaches to dealing with contaminant reaction times and parameter
#'  variability. \emph{Psychonomic Bulletin & Review, 9}(3), 438-481.
#' @export

nll <- function(par, rt, response, bound_index, drift_index, resid_index,
                sv_index = NULL, sw_index = NULL, st0_index = NULL, ...) {

  # Extract Parameters ------------------------------------------------------

  a <- par[paste0("a[", bound_index, "]")]
  v <- par[paste0("v[", drift_index, "]")]
  w <- par[paste0("w[", bound_index, "]")]
  t0 <- par[paste0("t0[", resid_index, "]")]
  sv <- if (is.na(par["sv[1]"])) 0 else par[paste0("sv[", sv_index, "]")]
  sw <- if (is.na(par["sw[1]"])) 0 else par[paste0("sw[", sw_index, "]")]
  st0 <- if (is.na(par["st0[1]"])) 0 else par[paste0("st0[", st0_index, "]")]

  # Evaluate PDF and Return Negative Log Likelihood -------------------------

  eval_pdf <- try(WienR::WienerPDF(
    t = rt, response = response, a = a, v = v, w = w, t0 = t0,
    sv = sv, sw = sw, st0 = st0, ...
  ), silent = TRUE)

  if (inherits(eval_pdf, "try-error")) return(Inf)
  return(-sum(eval_pdf$logvalue))
}


#' Gradient of Negative Log Likelihood
#'
#' Computes the gradient (first derivatives) of the negative log likelihood
#' with respect to each diffusion model parameter. Enables gradient-based
#' optimization methods for improved parameter estimation efficiency.
#'
#' @inheritParams nll
#'
#' @returns Named numeric vector of gradients with the same length and names as
#'   `par`. Returns vector of `NaN` values if gradient evaluation fails.
#' @references
#' Ratcliff, R., & Tuerlinckx, F. (2002). Estimating parameters of the diffusion
#'  model: Approaches to dealing with contaminant reaction times and parameter
#'  variability. \emph{Psychonomic Bulletin & Review, 9}(3), 438-481.
#' @export

gradient <- function(par, rt, response, bound_index, drift_index, resid_index,
                     sv_index = NULL, sw_index = NULL, st0_index = NULL, ...) {

  # Extract Parameters and Determine Which Are Used ------------------------

  a <- par[paste0("a[", bound_index, "]")]
  v <- par[paste0("v[", drift_index, "]")]
  w <- par[paste0("w[", bound_index, "]")]
  t0 <- par[paste0("t0[", resid_index, "]")]

  use_sv <- !is.na(par["sv[1]"])
  use_sw <- !is.na(par["sw[1]"])
  use_st0 <- !is.na(par["st0[1]"])

  sv <- if (use_sv) par[paste0("sv[", sv_index, "]")] else 0
  sw <- if (use_sw) par[paste0("sw[", sw_index, "]")] else 0
  st0 <- if (use_st0) par[paste0("st0[", st0_index, "]")] else 0

  # Evaluate Gradient and PDF -----------------------------------------------

  eval_grad <- try(WienR::gradWienerPDF(
    t = rt, response = response, a = a, v = v, w = w, t0 = t0,
    sv = sv, sw = sw, st0 = st0, ...
  ), silent = TRUE)

  if (inherits(eval_grad, "try-error")) return(rep(NaN, length(par)))

  eval_pdf <- try(WienR::WienerPDF(
    t = rt, response = response, a = a, v = v, w = w, t0 = t0,
    sv = sv, sw = sw, st0 = st0, ...
  ), silent = TRUE)

  if (inherits(eval_pdf, "try-error")) return(rep(NaN, length(par)))

  # Compute Gradients -------------------------------------------------------

  # Derivative of log(f(x)) is f'(x) / f(x)
  grad <- rep(NaN, length(par))
  names(grad) <- names(par)

  # Aggregate gradients by parameter index
  aggregate_grad <- function(param_prefix, deriv_col, index_vec) {
    for (i in seq_len(max(index_vec))) {
      grad[paste0(param_prefix, "[", i, "]")] <<-
        sum(eval_grad$deriv[index_vec == i, deriv_col] /
              eval_pdf$value[index_vec == i])
    }
  }

  aggregate_grad("a", "da", bound_index)
  aggregate_grad("w", "dw", bound_index)
  aggregate_grad("v", "dv", drift_index)
  aggregate_grad("t0", "dt0", resid_index)
  if (use_sv) aggregate_grad("sv", "dsv", sv_index)
  if (use_sw) aggregate_grad("sw", "dsw", sw_index)
  if (use_st0) aggregate_grad("st0", "dst0", st0_index)

  return(-grad)
}


#' EZ Diffusion Model Parameter Estimation
#'
#' Calculates closed-form estimates of simplified diffusion model parameters
#' from summary statistics. Provides fast initial parameter estimates by
#' assuming no trial-to-trial variability and unbiased starting point
#' (Wagenmakers et al., 2007). Useful for starting values in full model fitting.
#'
#' @param prop_correct Proportion of correct responses (0 to 1).
#' @param rt_correct_variance_seconds Variance of correct response times (s²).
#' @param rt_correct_mean_seconds Mean correct response time (s).
#' @param n_trials Number of trials (used for edge correction).
#' @param s Diffusion scaling parameter (default = 1). Traditionally fixed at
#'   0.1 or 1 depending on convention.
#'
#' @returns Named numeric vector with elements:
#'   \item{a}{Boundary separation}
#'   \item{v}{Drift rate}
#'   \item{ter}{Nondecision time (encoding and motor response)}
#'
#' @references
#' Wagenmakers, E.-J., Van Der Maas, H. L. J., & Grasman, R. P. P. P. (2007).
#'  An EZ-diffusion model for response time and accuracy. \emph{Psychonomic
#'  Bulletin & Review, 14}(1), 3-22.
#' @export
#' @examples
#' ezddm(prop_correct = 0.802, rt_correct_variance_seconds = 0.112,
#'       rt_correct_mean_seconds = 0.723, n_trials = 100)

ezddm <- function(prop_correct, rt_correct_variance_seconds,
                  rt_correct_mean_seconds, n_trials, s = 1) {

  # Apply Edge Correction ---------------------------------------------------

  s2 <- s^2

  # Edge correction for proportion correct
  prop_correct <- (2 * n_trials * prop_correct) / (2 * n_trials + 1) +
    1 / (4 * n_trials^2)

  # Calculate EZ Parameters -------------------------------------------------

  l <- stats::qlogis(prop_correct)
  x <- l * (l * prop_correct^2 - l * prop_correct + prop_correct - 0.5) /
    rt_correct_variance_seconds
  v <- sign(prop_correct - 0.5) * s * x^(1/4)
  a <- s2 * stats::qlogis(prop_correct) / v
  y <- -v * a / s2
  mdt <- (a / (2 * v)) * (1 - exp(y)) / (1 + exp(y))
  ter <- rt_correct_mean_seconds - mdt

  return(c(a = a, v = v, ter = ter))
}


#' Wiener Diffusion Model Quantile Function
#'
#' Computes quantiles of the response time distribution for a specified response
#' boundary by numerically inverting the Wiener diffusion model's cumulative
#' distribution function.
#'
#' @param p Quantile probability (0 to 1); e.g., 0.5 for median, 0.95 for 95th
#'   percentile.
#' @param response Response boundary indicator (typically 1 for lower, 2 for
#'   upper).
#' @param ... Model parameters passed to [WienR::WienerCDF()]: a (boundary
#'   separation), v (drift rate), w (starting point), t0 (nondecision time),
#'   and optionally sv, sw, st0 (trial-to-trial variability parameters).
#'
#' @returns Response time at the specified quantile. Returns `NA` if root-finding
#'   fails.
#'
#' @references
#' Ratcliff, R. (1978). A theory of memory retrieval. \emph{Psychological Review,
#'  85}(2), 59-108.
#' @export
#' @examples
#' \dontrun{
#' # Median RT for upper boundary responses
#' q_wdm(p = 0.5, response = 2, a = 1.5, v = 2, w = 0.5, t0 = 0.3)
#' }

q_wdm <- function(p, response, ...) {

  # Calculate Response Probability ------------------------------------------

  p_resp <- WienR::WienerCDF(Inf, response = response, ...)$value

  # Find Quantile via Root Finding ------------------------------------------

  res <- try(stats::uniroot(
    f = function(t) WienR::WienerCDF(t, response = response, ...)$value / p_resp - p,
    interval = c(0, 5),
    f.lower = -p,
    extendInt = "upX"
  ), silent = TRUE)

  if (inherits(res, "try-error")) NA else res$root
}

#' Null-Coalescing Operator
#'
#' Returns `y` if `x` is `NULL`, otherwise returns `x`.
#'
#' @name binary operator
#' @param x First value to check.
#' @param y Default value if `x` is `NULL`.
#'
#' @returns Either `x` (if not `NULL`) or `y`.
#' @keywords internal

`%||%` <- function(x, y){
  if (is.null(x)) y else x
}