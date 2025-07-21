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

  obs_rt_quantiles <- tibble(rt = rt, response = response,
                             drift_index = drift_index,
                             bound_index = bound_index,
                             resid_index = resid_index, sv_index = sv_index,
                             sw_index = sw_index, st0_index = st0_index) %>%
    group_by(drift_index, bound_index, resid_index, sv_index, sw_index,
             st0_index, response) %>%
    reframe(rt_q = quantile(rt, probs = rt_p)) %>%
    mutate(rt_p = rep(rt_p, n() / length(rt_p))) %>%
    complete(nesting(drift_index, bound_index, resid_index, sv_index, sw_index,
                     st0_index), response, rt_p, fill = list(rt_q = NA))

  obs_p_resp <- tibble(rt = rt, response = response, drift_index = drift_index,
                       bound_index = bound_index, resid_index = resid_index,
                       sv_index = sv_index, sw_index = sw_index,
                       st0_index = st0_index) %>%
    group_by(drift_index, bound_index, resid_index, sv_index, sw_index,
             st0_index, response) %>%
    summarize(n_resp = n(), .groups = "keep") %>%
    ungroup() %>%
    complete(nesting(drift_index, bound_index, resid_index, sv_index, sw_index,
                     st0_index), response, fill = list(n_resp = 0)) %>%
    group_by(drift_index, bound_index,
             resid_index, sv_index, sw_index, st0_index) %>%
    mutate(p_resp = n_resp / sum(n_resp))

  if (!is.null(par)) {
    par_names <- c(paste0("a[", 1:n_bound, "]"),
                   paste0("v[", 1:n_drift, "]"),
                   paste0("w[", 1:n_bound, "]"),
                   paste0("t0[", 1:n_resid, "]"),
                   paste0("sv[", 1:n_sv, "]"),
                   paste0("sw[", 1:n_sw, "]"),
                   paste0("st0[", 1:n_st0, "]"))

    par_to_use <- rep(0, length(par_names))
    names(par_to_use) <- par_names
    overlap <- intersect(names(par_to_use), names(par))
    par_to_use[overlap] <- par[overlap]

    fitDF <- expand_grid(nesting(drift_index, bound_index, resid_index,
                                 sv_index, sw_index, st0_index),
                         response = c("upper", "lower"), rt_p = rt_p) %>%
      mutate(rt_q = NA, p_resp = NA)

    for (i in 1:nrow(fitDF)) {
      fitDF$rt_q[i] <- q_wdm(p = fitDF$rt_p[i], response = fitDF$response[i],
                             a = par_to_use[paste0("a[", fitDF$bound_index[i], "]")],
                             v = par_to_use[paste0("v[", fitDF$drift_index[i], "]")],
                             w = par_to_use[paste0("w[", fitDF$bound_index[i], "]")],
                             t0 = par_to_use[paste0("t0[", fitDF$resid_index[i], "]")],
                             sv = par_to_use[paste0("sv[", fitDF$sv_index[i], "]")],
                             sw = par_to_use[paste0("sw[", fitDF$sw_index[i], "]")],
                             st0 = par_to_use[paste0("st0[", fitDF$st0_index[i], "]")])

      fitDF$p_resp[i] <- WienR::WienerCDF(t = Inf, response = fitDF$response[i],
                                          a = par_to_use[paste0("a[", fitDF$bound_index[i], "]")],
                                          v = par_to_use[paste0("v[", fitDF$drift_index[i], "]")],
                                          w = par_to_use[paste0("w[", fitDF$bound_index[i], "]")],
                                          t0 = par_to_use[paste0("t0[", fitDF$resid_index[i], "]")],
                                          sv = par_to_use[paste0("sv[", fitDF$sv_index[i], "]")],
                                          sw = par_to_use[paste0("sw[", fitDF$sw_index[i], "]")],
                                          st0 = par_to_use[paste0("st0[", fitDF$st0_index[i], "]")])$value
    }

    if (is.numeric(response)) {
      fitDF <- fitDF %>%
        mutate(response = as.numeric(factor(response,
                                            levels = c("lower", "upper"))))
    }

    obs_fit_data <- full_join(
      full_join(obs_p_resp, obs_rt_quantiles) %>%
        mutate(source = "Observed"),
      fitDF %>% mutate(source = "Fitted")
    )

    return(obs_fit_data)
  } else {
    return(full_join(obs_p_resp, obs_rt_quantiles))
  }
}