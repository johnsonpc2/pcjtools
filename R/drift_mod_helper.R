#' Drift Diffusion Simulation
#'
#' A helper function—used by the [drift_mod()] function—which runs a single
#'  simulation of a drift diffusion process based on the specified parameters.
#'  All model parameters should be numeric and of length 1, unless otherwise
#'  specified.
#'
#' @param v Mean drift rate. Determines the rate of evidence accumulation toward
#'  response boundaries and represents the quality of information extracted from
#'  the stimulus (Ratcliff & McKoon, 2008).
#' @param sv Standard deviation of drift rate. Allows drift rate to vary across
#'  trials from a normal distribution (Ratcliff & Rouder, 1998), accounting for
#'  trial-to-trial variability in information processing.
#' @param a Boundary separation. Represents response caution or the amount of
#'  evidence required before making a decision (Voss et al., 2004). Larger values
#'  indicate more cautious responding.
#' @param w Relative starting point (0-1). Represents a priori response bias,
#'  where values ≠ 0.5 indicate bias toward one boundary (Ratcliff & McKoon, 2008).
#' @param sw Starting point variability. Range of uniform distribution for
#'  trial-to-trial variation in starting point (Ratcliff & Rouder, 1998).
#' @param t0 Nondecision time. Duration of processes outside the decision stage,
#'  including stimulus encoding and motor response execution (Ratcliff & McKoon,
#'  2008).
#' @param st0 Range of nondecision time distribution. Allows nondecision time to
#'  vary uniformly across trials (Ratcliff & Tuerlinckx, 2002).
#' @param dt Time step increment for the simulation.
#' @param t_max Maximum simulation time before termination.
#' @returns A `'data.table'` object containing the time and activation of the
#' agent until it reached a boundary condition and the simulation ended.
#' @references
#' Ratcliff, R. (1978). A theory of memory retrieval. \emph{Psychological Review,
#'  85}(2), 59-108.
#'
#' Ratcliff, R., & McKoon, G. (2008). The diffusion decision model: Theory and
#'  data for two-choice decision tasks. \emph{Neural Computation, 20}(4), 873-922.
#'
#' Ratcliff, R., & Rouder, J. N. (1998). Modeling response times for two-choice
#'  decisions. \emph{Psychological Science, 9}(5), 347-356.
#'
#' Ratcliff, R., & Tuerlinckx, F. (2002). Estimating parameters of the diffusion
#'  model: Approaches to dealing with contaminant reaction times and parameter
#'  variability. \emph{Psychonomic Bulletin & Review, 9}(3), 438-481.
#'
#' Voss, A., Rothermund, K., & Voss, J. (2004). Interpreting the parameters of
#'  the diffusion model: An empirical validation. \emph{Memory & Cognition,
#'  32}(7), 1206-1220.
#' @keywords internal

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