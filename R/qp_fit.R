#' Title
#'
#' @param rt The RTs of decisions
#' @param response What the subject chose a trial
#' @param par Parameters
#' @param rt_p The percentiles to calculate from the RT distribution.
#' @param drift_index The index of the drift rate value.
#' @param bound_index The index of the boundary.
#' @param resid_index The index of residual
#' @param sv_index Trial-level variance of the draft rate
#' @param sw_index Variance in the starting position (I think)
#' @param st0_index Variance of the non-decision time
#'
#' @returns Something
#' @export
#'
#' @examples
#' \dontrun{
#' qp_fit(rt = data_sub$rt,
#' response = data_sub$correct,
#' par = ddm_fit$par)
#' }

qp_fit <- function(rt, response, par = NULL, rt_p = c(0.1, 0.3, 0.5, 0.7, 0.9),
                   drift_index = NULL, bound_index = NULL, resid_index = NULL,
                   sv_index = NULL, sw_index = NULL, st0_index = NULL) {

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

  # Calculate observed RT quantiles
  obs_rt_temp <- dplyr::tibble(
    rt = rt,
    response = response,
    drift_index = drift_index,
    bound_index = bound_index,
    resid_index = resid_index,
    sv_index = sv_index,
    sw_index = sw_index,
    st0_index = st0_index
  )

  obs_rt_grouped <- dplyr::group_by(
    obs_rt_temp,
    drift_index, bound_index, resid_index, sv_index, sw_index, st0_index, response
  )

  obs_rt_reframed <- dplyr::reframe(obs_rt_grouped, rt_q = stats::quantile(rt, probs = rt_p))

  obs_rt_mutated <- dplyr::mutate(obs_rt_reframed, rt_p = rep(rt_p, dplyr::n() / length(rt_p)))

  obs_rt_quantiles <- tidyr::complete(
    obs_rt_mutated,
    tidyr::nesting(drift_index, bound_index, resid_index, sv_index, sw_index, st0_index),
    response,
    rt_p,
    fill = list(rt_q = NA)
  )

  # Calculate observed response proportions
  obs_p_temp <- dplyr::tibble(
    rt = rt,
    response = response,
    drift_index = drift_index,
    bound_index = bound_index,
    resid_index = resid_index,
    sv_index = sv_index,
    sw_index = sw_index,
    st0_index = st0_index
  )

  obs_p_grouped <- dplyr::group_by(
    obs_p_temp,
    drift_index, bound_index, resid_index, sv_index, sw_index, st0_index, response
  )

  obs_p_summarized <- dplyr::summarize(obs_p_grouped, n_resp = dplyr::n(), .groups = "keep")

  obs_p_ungrouped <- dplyr::ungroup(obs_p_summarized)

  obs_p_completed <- tidyr::complete(
    obs_p_ungrouped,
    tidyr::nesting(drift_index, bound_index, resid_index, sv_index, sw_index, st0_index),
    response,
    fill = list(n_resp = 0)
  )

  obs_p_regrouped <- dplyr::group_by(
    obs_p_completed,
    drift_index, bound_index, resid_index, sv_index, sw_index, st0_index
  )

  obs_p_resp <- dplyr::mutate(obs_p_regrouped, p_resp = .data$n_resp / sum(.data$n_resp))

  if (!is.null(par)) {
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
    overlap <- dplyr::intersect(names(par_to_use), names(par))
    par_to_use[overlap] <- par[overlap]

    fitDF_temp <- tidyr::expand_grid(
      tidyr::nesting(drift_index, bound_index, resid_index, sv_index, sw_index, st0_index),
      response = c("upper", "lower"),
      rt_p = rt_p
    )

    fitDF <- dplyr::mutate(fitDF_temp, rt_q = NA, p_resp = NA)

    for (i in 1:nrow(fitDF)) {

      fitDF$rt_q[i] <- q_wdm(
        p = fitDF$rt_p[i],
        response = fitDF$response[i],
        a = par_to_use[paste0("a[", fitDF$bound_index[i], "]")],
        v = par_to_use[paste0("v[", fitDF$drift_index[i], "]")],
        w = par_to_use[paste0("w[", fitDF$bound_index[i], "]")],
        t0 = par_to_use[paste0("t0[", fitDF$resid_index[i], "]")],
        sv = par_to_use[paste0("sv[", fitDF$sv_index[i], "]")],
        sw = par_to_use[paste0("sw[", fitDF$sw_index[i], "]")],
        st0 = par_to_use[paste0("st0[", fitDF$st0_index[i], "]")]
      )

      fitDF$p_resp[i] <- WienR::WienerCDF(
        t = Inf,
        response = fitDF$response[i],
        a = par_to_use[paste0("a[", fitDF$bound_index[i], "]")],
        v = par_to_use[paste0("v[", fitDF$drift_index[i], "]")],
        w = par_to_use[paste0("w[", fitDF$bound_index[i], "]")],
        t0 = par_to_use[paste0("t0[", fitDF$resid_index[i], "]")],
        sv = par_to_use[paste0("sv[", fitDF$sv_index[i], "]")],
        sw = par_to_use[paste0("sw[", fitDF$sw_index[i], "]")],
        st0 = par_to_use[paste0("st0[", fitDF$st0_index[i], "]")]
      )$value
    }

    if (is.numeric(response)) {
      fitDF <- dplyr::mutate(
        fitDF,
        response = as.numeric(factor(.data$response, levels = c("upper", "lower")))
      )
    }

    fitDT <- data.table::setDT(fitDF)

    fitDT[, response := dplyr::case_when(
      .data$response == "upper" ~ 1,
      .data$response == "lower" ~ 0
    )]

    obs_joined <- dplyr::full_join(obs_p_resp, obs_rt_quantiles)
    obs_with_source <- dplyr::mutate(obs_joined, source = "Observed")
    fit_with_source <- dplyr::mutate(fitDT, source = "Fitted")
    obs_fit_data <- dplyr::full_join(obs_with_source, fit_with_source)

    return(obs_fit_data)
  } else {
    return(dplyr::full_join(obs_p_resp, obs_rt_quantiles))
  }
}