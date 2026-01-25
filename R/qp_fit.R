#' Quick Q–P (Quantile–Probability) Fit for the Drift–Diffusion Model
#'
#' Compute observed quantiles and choice probabilities from a data set and,
#' optionally, the corresponding predictions from a fitted drift–diffusion model.
#' The function is designed for the "quantile–probability" method advocated by
#' Ratcliff & Tuerlinckx (2002) and Ratcliff & McKoon (2008): RT quantiles
#' (default: 0.1, 0.3, 0.5, 0.7, 0.9) are plotted against the proportion of
#' choices for each condition, giving a compact graphical summary of accuracy,
#' speed, and distributional shape.
#'
#' @param rt Response-time vector (seconds).
#' @param response Choice vector: character ("upper"/"lower") or numeric (1/0).
#' @param par Named numeric vector with DDM parameters (see *Details*). If
#'   supplied, fitted quantiles/probabilities are appended to the output.
#' @param rt_p Numeric vector of quantiles to calculate (default =
#'   c(0.1, 0.3, 0.5, 0.7, 0.9)).
#' @param drift_index Trial-by-trial integer mapping to the drift-rate
#'   parameter (allows across-condition variability).
#' @param bound_index Trial-by-trial mapping to boundary-separation parameter.
#' @param resid_index Trial-by-trial mapping to non-decision-time parameter.
#' @param sv_index Trial-by-trial mapping to drift-rate variability parameter.
#' @param sw_index Trial-by-trial mapping to starting-point variability.
#' @param st0_index Trial-by-trial mapping to non-decision-time variability.
#'
#' @details
#' When `par` is provided, predicted quantiles are obtained with `q_wdm()`
#' (quantile function for the Wiener diffusion model) and choice probabilities
#' with `WienR::WienerCDF()`. Parameters should be named in the style produced
#' by `WienerDM` or `rtdists`: `a[1], v[1], w[1], t0[1], sv[1], sw[1], st0[1]`
#' (indices correspond to the mapping vectors above). Any parameter not
#' supplied defaults to 0 (or `a=1`, `w=0.5`).
#'
#' The function returns a `data.table` with one row per combination of
#' condition indices, response, and quantile; columns `rt_q` and `p_resp`
#' contain the observed (and, if `par` given, fitted) quantiles and choice
#' proportions. This object can be piped directly into `ggplot2` to produce a
#' quantile–probability plot (see examples).
#'
#' @return A `data.table` with columns:
#'   * `drift_index`, `bound_index`, `resid_index`, `sv_index`, `sw_index`, `st0_index`: Condition indices
#'   * `response`: "upper" or "lower"
#'   * `rt_p`: Quantile probability
#'   * `rt_q`: Observed RT quantile (seconds)
#'   * `p_resp`: Observed choice proportion
#'   * `source`: "Observed" or "Fitted" (when `par` supplied)
#'
#' @references
#' Ratcliff, R. (1978). A theory of memory retrieval. *Psychological Review*,
#' 85(2), 59–108. \doi{10.1037/0033-295X.85.2.59}
#'
#' Ratcliff, R., & McKoon, G. (2008). The diffusion decision model: Theory
#' and data for two-choice decision tasks. *Neural Computation*, 20(4),
#' 873–922. \doi{10.1162/neco.2008.12-06-420}
#'
#' Ratcliff, R., & Tuerlinckx, F. (2002). Estimating parameters of the
#' diffusion model: Approaches to dealing with contaminant reaction times and
#' parameter variability. *Psychonomic Bulletin & Review*, 9(3), 438–481.
#' \doi{10.3758/BF03196302}
#'
#' @export
#' @examples
#' \dontrun{
#' qp_fit(rt = data_sub$rt,
#' response = data_sub$correct,
#' par = ddm_fit$par)
#' }
#'

qp_fit <- function(rt, response, par = NULL, rt_p = c(0.1, 0.3, 0.5, 0.7, 0.9),
                   drift_index = NULL, bound_index = NULL, resid_index = NULL,
                   sv_index = NULL, sw_index = NULL, st0_index = NULL) {

  # Handle Indices ----------------------------------------------------------

  if (is.null(drift_index)) {
    drift_index <- rep(1, length(rt))
    n_drift <- 1
  } else {
    n_drift <- max(drift_index)
  }

  if (is.null(bound_index)) {
    bound_index <- rep(1, length(rt))
    n_bound <- 1
  } else {
    n_bound <- max(bound_index)
  }

  if (is.null(resid_index)) {
    resid_index <- rep(1, length(rt))
    n_resid <- 1
  } else {
    n_resid <- max(resid_index)
  }

  if (is.null(sv_index)) {
    sv_index <- rep(1, length(rt))
    n_sv <- 1
  } else {
    n_sv <- max(sv_index)
  }

  if (is.null(sw_index)) {
    sw_index <- rep(1, length(rt))
    n_sw <- 1
  } else {
    n_sw <- max(sw_index)
  }

  if (is.null(st0_index)) {
    st0_index <- rep(1, length(rt))
    n_st0 <- 1
  } else {
    n_st0 <- max(st0_index)
  }

  # Create Observed Data ----------------------------------------------------

  obs_dt <- data.table::data.table(
    rt = rt,
    response = response,
    drift_index = drift_index,
    bound_index = bound_index,
    resid_index = resid_index,
    sv_index = sv_index,
    sw_index = sw_index,
    st0_index = st0_index
  )

  # Calculate Observed RT Quantiles -----------------------------------------

  grp_vars <- c("drift_index", "bound_index", "resid_index",
                "sv_index", "sw_index", "st0_index", "response")

  obs_rt_quantiles <- obs_dt[,
                             .(rt_q = stats::quantile(rt, probs = rt_p)),
                             by = grp_vars
  ]

  # Add rt_p column
  obs_rt_quantiles[, rt_p := rep(rt_p, .N / length(rt_p))]

  # Calculate Observed Response Proportions ---------------------------------

  obs_p_resp <- obs_dt[, .(n_resp = .N), by = grp_vars]
  obs_p_resp[, p_resp := n_resp / sum(n_resp),
             by = c("drift_index", "bound_index", "resid_index",
                    "sv_index", "sw_index", "st0_index")]

  # Return Observed Only if No Parameters -----------------------------------

  if (is.null(par)) {
    result <- merge(obs_p_resp, obs_rt_quantiles, by = grp_vars, all = TRUE)
    return(result)
  }

  # Build Parameter Vector --------------------------------------------------

  par_names <- c(
    paste0("a[", 1:n_bound, "]"),
    paste0("v[", 1:n_drift, "]"),
    paste0("w[", 1:n_bound, "]"),
    paste0("t0[", 1:n_resid, "]"),
    paste0("sv[", 1:n_sv, "]"),
    paste0("sw[", 1:n_sw, "]"),
    paste0("st0[", 1:n_st0, "]")
  )

  par_to_use <- rep(0, length(par_names))
  names(par_to_use) <- par_names
  overlap <- intersect(names(par_to_use), names(par))
  par_to_use[overlap] <- par[overlap]

  # Create Prediction Grid --------------------------------------------------

  fit_dt <- data.table::CJ(
    drift_index = 1:n_drift,
    bound_index = 1:n_bound,
    resid_index = 1:n_resid,
    sv_index = 1:n_sv,
    sw_index = 1:n_sw,
    st0_index = 1:n_st0,
    response = c("upper", "lower"),
    rt_p = rt_p
  )

  # Calculate Fitted Quantiles and Probabilities ---------------------------

  fit_dt[, rt_q := q_wdm(
    p = rt_p,
    response = response,
    a = par_to_use[paste0("a[", bound_index, "]")],
    v = par_to_use[paste0("v[", drift_index, "]")],
    w = par_to_use[paste0("w[", bound_index, "]")],
    t0 = par_to_use[paste0("t0[", resid_index, "]")],
    sv = par_to_use[paste0("sv[", sv_index, "]")],
    sw = par_to_use[paste0("sw[", sw_index, "]")],
    st0 = par_to_use[paste0("st0[", st0_index, "]")]
  )]

  fit_dt[, p_resp := WienR::WienerCDF(
    t = Inf,
    response = response,
    a = par_to_use[paste0("a[", bound_index, "]")],
    v = par_to_use[paste0("v[", drift_index, "]")],
    w = par_to_use[paste0("w[", bound_index, "]")],
    t0 = par_to_use[paste0("t0[", resid_index, "]")],
    sv = par_to_use[paste0("sv[", sv_index, "]")],
    sw = par_to_use[paste0("sw[", sw_index, "]")],
    st0 = par_to_use[paste0("st0[", st0_index, "]")]
  )$value]

  # Handle Numeric Response -------------------------------------------------

  if (is.numeric(response)) {
    fit_dt[, response := as.numeric(factor(response, levels = c("upper", "lower")))]
  }

  # Combine Observed and Fitted ---------------------------------------------

  obs_combined <- merge(obs_p_resp, obs_rt_quantiles, by = grp_vars, all = TRUE)
  obs_combined[, source := "Observed"]
  fit_dt[, source := "Fitted"]

  result <- rbind(obs_combined, fit_dt, fill = TRUE)

  return(result)
}