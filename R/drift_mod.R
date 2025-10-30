#' Drift Diffusion Model
#'
#' A function to run and visualize the results of a drift diffusion model.
#'
#' @param nsims An integer; the number of simulations of the model to run.
#' @inheritParams diffusion_sim
#' @param plots Logical; should plot elements be generated?
#' @param ... optional arguments passed to `'theme_pcj()'`.
#'
#' @returns A list—including the results of the simulation—and plots for the
#' average evidence accumulation over time, the conditional rt distribution,
#' and the quantile-probability plot.
#'
#' @import data.table
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' drift_mod()

drift_mod <- function(nsims = 1000, v = 0.5, sv = 0, a = 2, w = 0.5, sw = 0.9,
                      t0 = 0, st0 = .2, dt = .01, t_max = Inf, plots = TRUE,
                      ...) {

# Initialize and Run Simulation -------------------------------------------

  # The number of simulations to run
  n_sims <- nsims

  # Initially empty, this will eventually save our random walk simulations
  sim_results <- c()

  # The for loop increments a counter over a specified range (from 1 to n_sims)
  for (i in 1:n_sims) {

    # Simulate a single realization of the random walk with the given parameters
    current_result <- diffusion_sim(v, sv, a, w, sw, t0, st0, dt, t_max)

    # "Bind" the current simulation to the ongoing record of simulation results
    sim_results <- rbind(
      sim_results,
      # Add a new column that identifies which simulation this was
      current_result[, sim_index := i]
    )

  }


# Visualize Simulation Results --------------------------------------------

  if (plots == TRUE) {

  # Extract simulated choices and RT's
  choice_rt <- sim_results[, `:=`(choice = ifelse(last(x) > 0, "upper", "lower"),
                                  rt = last(t)),
                           by = sim_index]

  # Quantile-probability plot
  sim_choice_p <- choice_rt[, .N, by = choice][, p_resp := N / sum(N)]

  qprobs <- c(0.1, 0.3, 0.5, 0.7, 0.9)

  sim_rt_q <- choice_rt[, list(rt_q = stats::quantile(x = rt, probs = qprobs)),
                        by = choice]

  # Visualize the internal evidence states
  evidence_plot <- ggplot(
    data = sim_results,
    mapping = aes(
      x = t,
      y = x)
  ) +
    stat_density2d_filled(show.legend = FALSE)

  fp1 <- theme_pcj(
    plot = evidence_plot,
    plot_text = c(title = "Internal Evidence States Over Time",
                  subtitle = "",
                  ylab = "Accumulated Evidence",
                  xlab = "Time",
                  fill = "Relative frequency"),
    ...
  )

  # Plot conditional RT distributions
 rt_dist_plot <-  ggplot(
    data = choice_rt,
    mapping = aes(
      x = rt,
      fill = choice,
      alpha = .1)
  ) +
    geom_density() +
   guides(alpha = "none")

 fp2 <- theme_pcj(
   plot = rt_dist_plot,
   plot_text = c(title = "Conditional RT Distributions",
                 subtitle = "",
                 ylab = "Frequency",
                 xlab = "Response Time",
                 color = "Choice"),
   ...
 )

  # Plot the quantiles of the upper and lower response distributions
  response_quantiles <- ggplot(
    data = dplyr::full_join(sim_choice_p, sim_rt_q, by = "choice"),
    mapping = aes(
      x = p_resp,
      y = rt_q,
      color = choice)
  ) +
    geom_point() +
    expand_limits(x = c(0, 1))

  fp3 <- theme_pcj(
    plot = response_quantiles,
    plot_text = c(title = "Quantile-Probability Plot",
                  subtitle = "",
                  ylab = "RT Quantile",
                  xlab = "Response Proportion")
  )

  } else {
    fp1 = NULL
    fp2 = NULL
    fp3 = NULL
  }

  results <- list(
    "sim_results" = sim_results,
    "Evidence Plot" = fp1,
    "RT Dist" = fp2,
    "Quantile Plot" = fp3
  )

  filtered_results <- list()
  for (name in names(results)) {
    if (!is.null(results[[name]])) {
      filtered_results[[name]] <- results[[name]]
    }
  }

  invisible(filtered_results)

}