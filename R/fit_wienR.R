#' Title
#'
#' @param fit_sv
#' @param fit_sw
#' @param fit_st0
#' @param optim_control
#' @param init_par
#' @inheritParams nll
#'
#' @returns
#' @export
#'
#' @examples
fit_wienr <- function(rt, response, fit_sv = FALSE, fit_sw = FALSE,
                      fit_st0 = FALSE, optim_control = list(), init_par = NULL,
                      drift_index = NULL, bound_index = NULL, resid_index = NULL,
                      sv_index = NULL, sw_index = NULL, st0_index = NULL) {

  if (!is.factor(response)) response <- as.factor(response)
  response_numeric <- as.numeric(response)

  par_names <- c()

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

  par_names <- c(
    paste0("a[", 1:n_bound, "]"), paste0("v[", 1:n_drift, "]"),
    paste0("w[", 1:n_bound, "]"), paste0("t0[", 1:n_resid, "]")
  )

  if (fit_sv) par_names <- c(par_names, paste0("sv[", 1:n_sv, "]"))
  if (fit_sw) par_names <- c(par_names, paste0("sw[", 1:n_sw, "]"))
  if (fit_st0) par_names <- c(par_names, paste0("st0[", 1:n_st0, "]"))

  init_to_use <- rep(NA, length(par_names))
  names(init_to_use) <- par_names

  ez_init <- ezddm(
    prop_correct = mean(response_numeric == 2),
    rt_correct_variance_seconds = var(rt[response_numeric == 2]),
    rt_correct_mean_seconds = mean(rt[response_numeric == 2]),
    n_trials = length(response_numeric)
  )

  init_to_use[startsWith(par_names, "a[")] <- unname(ez_init["a"])
  init_to_use[startsWith(par_names, "v[")] <- unname(ez_init["v"])
  init_to_use[startsWith(par_names, "w[")] <- 0.5
  init_to_use[startsWith(par_names, "t0[")] <- unname(min(0.99 * min(rt),
                                                          ez_init["Ter"]))

  if (fit_sv) init_to_use[startsWith(par_names, "sv[")] <- 0
  if (fit_sw) init_to_use[startsWith(par_names, "sw[")] <- 0
  if (fit_st0) init_to_use[startsWith(par_names, "st0[")] <- 0

  if (!is.null(init_par)) {

    overlap <- intersect(names(init_to_use), names(init_par))
    init_to_use[overlap] <- init_par[overlap]

  }

  init_par <- init_to_use

  lower <- rep(-Inf, length(par_names))
  upper <- rep(Inf, length(par_names))

  names(lower) <- names(upper) <- par_names

  lower[startsWith(par_names, "a[")] <- 0
  lower[startsWith(par_names, "w[")] <- 0
  lower[startsWith(par_names, "t0[")] <- 0
  lower[startsWith(par_names, "sv[")] <- 0
  lower[startsWith(par_names, "sw[")] <- 0
  lower[startsWith(par_names, "st0[")] <- 0

  upper[startsWith(par_names, "w[")] <- 1
  upper[startsWith(par_names, "t0[")] <- unname(tapply(rt,
                                                       INDEX = resid_index,
                                                       FUN = min))
  upper[startsWith(par_names, "sw[")] <- 1

  if (!is.na(init_par["sw[1]"]) || !is.na(init_par["st0[1]"])) {
    init_par0 <- init_par[!(startsWith(names(init_par), "sw") ||
                              startsWith(names(init_par), "st0"))]

    fit0 <- try(optim(
      par = init_par0,
      fn = nll,
      gr = gradient,
      method = "Nelder-Mead",
      control = c(optim_control, maxit = 10000),
      rt = rt,
      response = response_numeric,
      bound_index = bound_index,
      drift_index = drift_index,
      resid_index = resid_index,
      sv_index = sv_index,
      sw_index = sw_index,
      st0_index = st0_index
    ), silent = TRUE)

    if (class(fit0) != "try-error") {
      overlap <- intersect(names(init_par), names(init_par0))
      init_par[overlap] <- fit0$par[overlap]
    }
  }

  fit1 <- optim(
    par = init_par,
    fn = nll,
    gr = gradient,
    method = "Nelder-Mead",
    control = c(optim_control, maxit = 10000),
    rt = rt,
    response = response_numeric,
    bound_index = bound_index,
    drift_index = drift_index,
    resid_index = resid_index,
    sv_index = sv_index,
    sw_index = sw_index,
    st0_index = st0_index
  )

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