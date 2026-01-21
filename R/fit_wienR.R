#' Fit Wiener Diffusion Model via Maximum Likelihood
#'
#' Estimates Wiener diffusion model parameters from response time and choice
#' data using maximum likelihood estimation (Ratcliff & Tuerlinckx, 2002).
#' Employs a multi-stage optimization strategy: Nelder-Mead for global search
#' followed by quasi-Newton (ucminf) for local refinement, combining the
#' strengths of both approaches for robust parameter recovery.
#'
#' @param rt Numeric vector of response times (seconds).
#' @param response Factor or numeric vector of responses. Converted to numeric
#'   if necessary (typically 1 for lower, 2 for upper boundary).
#' @param fit_sv Logical. If `TRUE`, estimates trial-to-trial drift rate
#'   variability (sv). Default is `FALSE`.
#' @param fit_sw Logical. If `TRUE`, estimates trial-to-trial starting point
#'   variability (sw). Default is `FALSE`.
#' @param fit_st0 Logical. If `TRUE`, estimates trial-to-trial nondecision time
#'   variability (st0). Default is `FALSE`.
#' @param optim_control List of control parameters passed to [stats::optim()]
#'   and [ucminf::ucminf()]. See respective documentation for options.
#' @param init_par Named vector of initial parameter values (optional). If
#'   `NULL`, computed via EZ-diffusion (Wagenmakers et al., 2007). Parameter
#'   names should follow format: `a[1]`, `v[1]`, `w[1]`, `t0[1]`, etc.
#' @param drift_index Integer vector indicating which drift rate parameter each
#'   trial uses. If `NULL`, all trials use same parameter. Length must equal
#'   length of `rt`.
#' @param bound_index Integer vector indicating which boundary separation (a)
#'   and starting point (w) each trial uses. If `NULL`, all trials use same
#'   parameters. Length must equal length of `rt`.
#' @param resid_index Integer vector indicating which nondecision time (t0)
#'   each trial uses. If `NULL`, all trials use same parameter. Length must
#'   equal length of `rt`.
#' @param sv_index Integer vector indicating which drift rate variability (sv)
#'   each trial uses. Only relevant if `fit_sv = TRUE`. If `NULL`, all trials
#'   use same parameter. Length must equal length of `rt`.
#' @param sw_index Integer vector indicating which starting point variability
#'   (sw) each trial uses. Only relevant if `fit_sw = TRUE`. If `NULL`, all
#'   trials use same parameter. Length must equal length of `rt`.
#' @param st0_index Integer vector indicating which nondecision time variability
#'   (st0) each trial uses. Only relevant if `fit_st0 = TRUE`. If `NULL`, all
#'   trials use same parameter. Length must equal length of `rt`.
#'
#' @returns List object from [ucminf::ucminf()] containing:
#'   \item{par}{Fitted parameter values}
#'   \item{value}{Negative log-likelihood at minimum}
#'   \item{convergence}{Convergence code (1 = success)}
#'   \item{message}{Convergence message}
#'   See [ucminf::ucminf()] documentation for complete details.
#'
#' @references
#' Nielsen, H. B. (2000). UCMINF - An algorithm for unconstrained, nonlinear
#'  optimization. Report IMM-REP-2000-19, Department of Mathematical Modelling,
#'  Technical University of Denmark.
#'
#' Ratcliff, R., & Tuerlinckx, F. (2002). Estimating parameters of the diffusion
#'  model: Approaches to dealing with contaminant reaction times and parameter
#'  variability. \emph{Psychonomic Bulletin & Review, 9}(3), 438-481.
#'
#' Wagenmakers, E.-J., Van Der Maas, H. L. J., & Grasman, R. P. P. P. (2007).
#'  An EZ-diffusion model for response time and accuracy. \emph{Psychonomic
#'  Bulletin & Review, 14}(1), 3-22.
#' @export

fit_wienr <- function(rt, response, fit_sv = FALSE, fit_sw = FALSE,
                      fit_st0 = FALSE, optim_control = list(), init_par = NULL,
                      drift_index = NULL, bound_index = NULL, resid_index = NULL,
                      sv_index = NULL, sw_index = NULL, st0_index = NULL) {

  # Prepare Data and Indices ------------------------------------------------

  # Convert response to numeric if needed
  if (!is.factor(response)) response <- as.factor(response)
  response_numeric <- as.numeric(response)

  # Set default indices if not provided
  n <- length(rt)
  drift_index <- drift_index %||% rep(1, n)
  bound_index <- bound_index %||% rep(1, n)
  resid_index <- resid_index %||% rep(1, n)
  sv_index <- sv_index %||% rep(1, n)
  sw_index <- sw_index %||% rep(1, n)
  st0_index <- st0_index %||% rep(1, n)

  # Get parameter counts
  n_drift <- max(drift_index)
  n_bound <- max(bound_index)
  n_resid <- max(resid_index)
  n_sv <- max(sv_index)
  n_sw <- max(sw_index)
  n_st0 <- max(st0_index)

  # Build Parameter Structure -----------------------------------------------

  # Build parameter names
  par_names <- c(
    paste0("a[", seq_len(n_bound), "]"),
    paste0("v[", seq_len(n_drift), "]"),
    paste0("w[", seq_len(n_bound), "]"),
    paste0("t0[", seq_len(n_resid), "]")
  )
  if (fit_sv) par_names <- c(par_names, paste0("sv[", seq_len(n_sv), "]"))
  if (fit_sw) par_names <- c(par_names, paste0("sw[", seq_len(n_sw), "]"))
  if (fit_st0) par_names <- c(par_names, paste0("st0[", seq_len(n_st0), "]"))

  # Initialize Parameters via EZ-Diffusion ----------------------------------

  # Use EZ-diffusion for starting values
  ez_init <- ezddm(
    prop_correct = mean(response_numeric == 2),
    rt_correct_variance_seconds = stats::var(rt[response_numeric == 2]),
    rt_correct_mean_seconds = mean(rt[response_numeric == 2]),
    n_trials = length(response_numeric)
  )

  init_to_use <- c(
    rep(ez_init["a"], n_bound),
    rep(ez_init["v"], n_drift),
    rep(0.5, n_bound),
    rep(min(0.99 * min(rt), ez_init["ter"]), n_resid)
  )
  if (fit_sv) init_to_use <- c(init_to_use, rep(0, n_sv))
  if (fit_sw) init_to_use <- c(init_to_use, rep(0, n_sw))
  if (fit_st0) init_to_use <- c(init_to_use, rep(0, n_st0))

  names(init_to_use) <- par_names

  # Override with user-provided initial values
  if (!is.null(init_par)) {
    overlap <- intersect(names(init_to_use), names(init_par))
    init_to_use[overlap] <- init_par[overlap]
  }

  # Set Parameter Bounds ----------------------------------------------------

  lower <- ifelse(grepl("^(a|w|t0|sv|sw|st0)\\[", par_names), 0, -Inf)
  upper <- ifelse(grepl("^(w|sw)\\[", par_names), 1, Inf)
  names(lower) <- names(upper) <- par_names

  # Set t0 upper bounds to minimum RT per group
  upper[startsWith(par_names, "t0[")] <- tapply(rt, resid_index, min)

  # Stage 1: Simplified Model (if fitting sw or st0) -----------------------

  # Two-stage optimization if fitting sw or st0
  if (fit_sw || fit_st0) {
    # Fit model without sw/st0 first for better initialization
    init_par0 <- init_to_use[!grepl("^(sw|st0)\\[", names(init_to_use))]

    fit0 <- try(stats::optim(
      par = init_par0,
      fn = nll,
      gr = gradient,
      method = "Nelder-Mead",
      control = c(optim_control, list(maxit = 10000)),
      rt = rt,
      response = response_numeric,
      bound_index = bound_index,
      drift_index = drift_index,
      resid_index = resid_index,
      sv_index = sv_index,
      sw_index = sw_index,
      st0_index = st0_index
    ), silent = TRUE)

    if (!inherits(fit0, "try-error")) {
      overlap <- intersect(names(init_to_use), names(fit0$par))
      init_to_use[overlap] <- fit0$par[overlap]
    }
  }

  # Stage 2: Nelder-Mead Global Optimization --------------------------------

  fit1 <- stats::optim(
    par = init_to_use,
    fn = nll,
    gr = gradient,
    method = "Nelder-Mead",
    control = c(optim_control, list(maxit = 10000)),
    rt = rt,
    response = response_numeric,
    bound_index = bound_index,
    drift_index = drift_index,
    resid_index = resid_index,
    sv_index = sv_index,
    sw_index = sw_index,
    st0_index = st0_index
  )

  # Stage 3: Quasi-Newton Local Refinement ----------------------------------

  fit2 <- ucminf::ucminf(
    par = fit1$par,
    fn = nll,
    gr = gradient,
    control = optim_control,
    rt = rt,
    response = response_numeric,
    bound_index = bound_index,
    drift_index = drift_index,
    resid_index = resid_index,
    sv_index = sv_index,
    sw_index = sw_index,
    st0_index = st0_index
  )

  return(fit2)
}