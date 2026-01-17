#' Drift Diffusion Simulation
#'
#' A helper function—used by the [drift_mod()] function—which runs a single
#' simulation of a drift diffusion process based on the specified parameters.
#' All model parameters should be numeric and of length 1, unless otherwise
#' specified.
#'
#' @param v The average drift rate. Determines the rate the agent travels
#'  towards evidence boundaries. When multiple trials are simulated, drift rates
#'  are randomly sampled from a normal distribution with a mean of `v`.
#' @param sv The standard deviation of the drift rate. When multiple simulations
#'  are run and `sv` > 0, drift rates change trial by trial based on random
#'  sampling from a normal distribution.
#' @param a Response caution, represented by the distance between evidence
#'  boundaries and the agent's starting point. Larger distances between the
#'  starting point and decision threshold represent more caution as more evidence
#'  is required for the agent to come to a decision.
#' @param w Average response bias. the avg ease of traveling to either boundary
#' @param sw Boundary/starting point deviation
#' @param t0 Residual time: time lag between evidence accumulation and action
#' @param st0 The mean of the residual time distribution
#' @param dt Delta t: the units by which time increments
#' @param t_max Maximum time: the max time the model will draw samples
#'
#' @returns A `'data.table'` object containing the time and activation of the
#' agent until it reached a boundary condition and the simulation ended.
#'
#' @export
#'
#' @examples
#' diffusion_sim()

diffusion_sim <- function(v = 0, sv = 0, a = 2, w = 0.5, sw = 0,
                          t0 = 0.2, st0 = 0, dt = 0.01, t_max = Inf) {

  # See notes Section 4.3 on parameter considerations for slow and fast errors

  trial_v <- stats::rnorm(n = 1, mean = v, sd = sv)
  trial_w <- stats::runif(n = 1,
                          min = max(0, w - 0.5 * sw),
                          max = min(1, w + 0.5 * sw)
  )
  trial_t0 <- stats::runif(n = 1, min = t0, max = t0 + st0)

  b_upper <- (1 - trial_w) * a
  b_lower <- -trial_w * a

  x <- 0
  t <- trial_t0

  x_record <- x
  t_record <- t

  while (x < b_upper && x > b_lower && t < t_max) {
    x_sample <- stats::rnorm(n = 1, mean = trial_v * dt, sd = 1 * sqrt(dt))
    x <- x + x_sample
    t <- t + dt
    x_record <- c(x_record, x)
    t_record <- c(t_record, t)
  }

  return(data.table::data.table(t = t_record, x = x_record))
}