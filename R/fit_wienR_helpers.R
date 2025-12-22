#' Find The Negative Log Likelihood
#'
#' A helper function used by fit_wienr() to calculate the negative log
#' likelihood of a set of diffusion model parameters given observed data.
#'
#' @param par Named numeric vector of parameters. Names should follow the format
#'   `a[i]`, `v[i]`, `w[i]`, `t0[i]`, `sv[i]`, `sw[i]`, `st0[i]` where i is the
#'   parameter index.
#' @param rt Numeric vector of response times in seconds.
#' @param response Numeric vector of responses (typically 1 or 2, representing
#'   lower or upper boundary).
#' @param bound_index Integer vector mapping each trial to its boundary separation
#'   (a) and starting point (w) parameter index.
#' @param drift_index Integer vector mapping each trial to its drift rate (v)
#'   parameter index.
#' @param resid_index Integer vector mapping each trial to its non-decision time
#'   (t0) parameter index.
#' @param sv_index Integer vector mapping each trial to its drift rate variability
#'   (sv) parameter index. Can be NULL if sv is not being estimated.
#' @param sw_index Integer vector mapping each trial to its starting point
#'   variability (sw) parameter index. Can be NULL if sw is not being estimated.
#' @param st0_index Integer vector mapping each trial to its non-decision time
#'   variability (st0) parameter index. Can be NULL if st0 is not being estimated.
#' @param ... Additional arguments passed to [WienR::WienerPDF()].
#'
#' @returns A single numeric value representing the negative log likelihood.
#'   Returns Inf if the PDF evaluation fails (e.g., invalid parameters).
#' @export

nll <- function(par, rt, response, bound_index, drift_index, resid_index,
                sv_index = NULL, sw_index = NULL, st0_index = NULL, ...) {

  # Extract parameters
  a <- par[paste0("a[", bound_index, "]")]
  v <- par[paste0("v[", drift_index, "]")]
  w <- par[paste0("w[", bound_index, "]")]
  t0 <- par[paste0("t0[", resid_index, "]")]
  sv <- if (is.na(par["sv[1]"])) 0 else par[paste0("sv[", sv_index, "]")]
  sw <- if (is.na(par["sw[1]"])) 0 else par[paste0("sw[", sw_index, "]")]
  st0 <- if (is.na(par["st0[1]"])) 0 else par[paste0("st0[", st0_index, "]")]

  # Evaluate PDF
  eval_pdf <- try(WienR::WienerPDF(
    t = rt, response = response, a = a, v = v, w = w, t0 = t0,
    sv = sv, sw = sw, st0 = st0, ...
  ), silent = TRUE)

  if (inherits(eval_pdf, "try-error")) return(Inf)
  return(-sum(eval_pdf$logvalue))
}


#' Compute Gradient of Negative Log Likelihood
#'
#' Calculates the gradient (first derivative) of the negative log likelihood
#' with respect to each model parameter. Used for gradient-based optimization
#' in fit_wienr().
#'
#' @inheritParams nll
#'
#' @returns A named numeric vector of gradients with the same length and names
#'   as par. Returns a vector of NaN values if gradient evaluation fails.
#' @export

gradient <- function(par, rt, response, bound_index, drift_index, resid_index,
                     sv_index = NULL, sw_index = NULL, st0_index = NULL, ...) {

  # Extract parameters and determine which are used
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

  # Evaluate gradient and PDF
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

  # Compute gradients (derivative of log(f(x)) is f'(x) / f(x))
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


#' EZ Diffusion Model Estimation
#'
#' Calculates simple, closed-form estimates of diffusion model parameters
#' from summary statistics. This provides a quick approximation useful for
#' initial parameter values. Based on the EZ-diffusion method.
#'
#' @param prop_correct Proportion of correct responses (between 0 and 1).
#' @param rt_correct_variance_seconds Variance of response times for correct
#'   responses, in seconds.
#' @param rt_correct_mean_seconds Mean response time for correct responses,
#'   in seconds.
#' @param n_trials Total number of trials (used for edge correction).
#' @param s Scaling parameter for the diffusion process (default = 1).
#'   Traditionally set to 1 or 0.1 depending on the scaling convention.
#'
#' @returns A named numeric vector with three elements:
#'   \item{a}{Boundary separation (distance between response boundaries)}
#'   \item{v}{Drift rate (rate of evidence accumulation)}
#'   \item{ter}{Non-decision time (encoding and motor response time)}
#'
#' @export
#' @examples
#' ezddm(prop_correct = 0.802, rt_correct_variance_seconds = 0.112,
#'       rt_correct_mean_seconds = 0.723, n_trials = 100)

ezddm <- function(prop_correct, rt_correct_variance_seconds,
                  rt_correct_mean_seconds, n_trials, s = 1) {

  s2 <- s^2

  # Edge correction for proportion correct
  prop_correct <- (2 * n_trials * prop_correct) / (2 * n_trials + 1) +
    1 / (4 * n_trials^2)

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


#' Quantile Function for Wiener Diffusion Model
#'
#' Computes quantiles of the response time distribution for a given response
#' boundary in the Wiener diffusion model. Uses root-finding to invert the
#' cumulative distribution function.
#'
#' @param p Numeric value between 0 and 1 representing the desired quantile
#'   (e.g., 0.5 for median, 0.95 for 95th percentile).
#' @param response Numeric value indicating which response boundary (typically
#'   1 for lower, 2 for upper).
#' @param ... Additional arguments passed to [WienR::WienerCDF()], such as
#'   model parameters (a, v, w, t0, sv, sw, st0).
#'
#' @returns A numeric value representing the response time at the specified
#'   quantile for the given response boundary. Returns NA if root-finding fails.
#'
#' @export
#' @examples
#' \dontrun{
#' # Find the median RT for upper boundary responses
#' q_wdm(p = 0.5, response = 2, a = 1.5, v = 2, w = 0.5, t0 = 0.3)
#' }

q_wdm <- function(p, response, ...) {
  p_resp <- WienR::WienerCDF(Inf, response = response, ...)$value

  res <- try(stats::uniroot(
    f = function(t) WienR::WienerCDF(t, response = response, ...)$value / p_resp - p,
    interval = c(0, 5),
    f.lower = -p,
    extendInt = "upX"
  ), silent = TRUE)

  if (inherits(res, "try-error")) NA else res$root
}

#' Helper function for null-coalescing operator
#'
#' If x is null, y. Otherwise, x.
#'
#' @name binary operator
#' @param x Condition 1
#' @param y Condition 2
#'
#' @keywords internal
`%||%` <- function(x, y){

  if (is.null(x)) y else x

}