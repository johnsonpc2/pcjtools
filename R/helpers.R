#' Drift Diffusion Simulation
#'
#' A helper function—used by the `'drift_mod()'` function—which runs a single
#' simulation of a drift diffusion process based on the specified parameters.
#' All model parameters should be numeric and of length 1, unless otherwise
#' specified.
#'
#' @param v The average drift rate towards a boundary.
#' @param sv Drift rate standard deviation: variability of the sampled drift
#'  rate. When multiple simulations are run, such as with the `'drift_mod()'`
#'  function, this allows drift rates to change from one simulation to the next.
#' @param a Response caution: the threshold to come to a decision represented
#' by the distance between the starting point and the
#' @param w Response bias mean: the avg ease of traveling to either boundary
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
  trial_w <- stats::runif(n = 1, min = max(0, w - 0.5 * sw),
                          max = min(1, w + 0.5 * sw))
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



#' Fit a Diffusion Model
#'
#' This function will fit a diffusion model to subjects' aggregated data.
#'
#' @param prop_correct Proportion correct.
#' @param rt_correct_variance_seconds The variance of the correct response times
#'  (in seconds).
#' @param rt_correct_mean_seconds The mean of correct response times (in
#' seconds).
#' @param nTrials The number of trials.
#' @param s A scaling parameter (often set to 1.0—the default—or 0.1).
#'
#' @returns A vector containing `'a'` (boundary),
#' `'v'` (drift rate), and `'ter'` (non-decision time) parameters.
#'
#' @export
#'
#' @examples
#' ezddm(prop_correct = .802, rt_correct_variance_seconds = .112,
#' rt_correct_mean_seconds = .723, n_trials = 100)

ezddm <- function(prop_correct, rt_correct_variance_seconds,
                  rt_correct_mean_seconds, n_trials, s = 1) {

  # Function adapted from the function of the same name in the `fddm` package
  s2 <- s^2
  v <- as.numeric(NA)
  a <- as.numeric(NA)
  ter <- as.numeric(NA)

  prop_correct <- (2 * n_trials * prop_correct) /
    (2 * n_trials + 1) + 1 / (4 * n_trials * n_trials)
  l <- stats::qlogis(prop_correct)
  x <- l * (l * prop_correct^2 - l * prop_correct + prop_correct - 0.5) /
    rt_correct_variance_seconds
  v <- sign(prop_correct - 0.5) * s * x^(1 / 4)
  a <- s2 * stats::qlogis(prop_correct) / v
  y <- -v * a / s2
  mdt <- (a / (2 * v)) * (1 - exp(y)) / (1 + exp(y))
  ter <- rt_correct_mean_seconds - mdt
  return(c("a" = a, "v" = v, "ter" = ter))
}



#' Title
#'
#' @param par Start
#' @param rt Start
#' @param response Start
#' @param bound_index Start
#' @param drift_index Start
#' @param resid_index Start
#' @param sv_index Start
#' @param sw_index Start
#' @param st0_index Start
#' @param ... Optional arguments to pass to `'WienR'` package functions.
#'
#' @returns A vector of gradients
#'
#' @export
#'
#' @examples
#' gradient(par, rt, response, bound_index, drift_index, resid_index)
gradient <- function(par, rt, response, bound_index, drift_index, resid_index,
                     sv_index = NULL, sw_index = NULL, st0_index = NULL, ...) {

  a <- par[paste0("a[", bound_index, "]")]
  v <- par[paste0("v[", drift_index, "]")]
  w <- par[paste0("w[", bound_index, "]")]
  t0 <- par[paste0("t0[", resid_index, "]")]

  if (is.na(par["sv[1]"])) {
    sv <- 0
    use_sv <- FALSE
  } else {
    sv <- par[paste0("sv[", sv_index, "]")]
    use_sv <- TRUE
  }

  if (is.na(par["sw[1]"])) {
    sw <- 0
    use_sw <- FALSE
  } else {
    sw <- par[paste0("sw[", sw_index, "]")]
    use_sw <- TRUE
  }

  if (is.na(par["st0[1]"])) {
    st0 <- 0
    use_st0 <- FALSE
  } else {
    st0 <- par[paste0("st0[", st0_index, "]")]
    use_st0 <- TRUE
  }

  eval_grad <- try(WienR::gradWienerPDF(t = rt, response = response, a = a,
                                        v = v, w = w, t0 = t0, sv = sv, sw = sw,
                                        st0 = st0, ...),
                   silent = TRUE)

  if (any(class(eval_grad) == "try-error")) return(rep(NaN, length(par)))

  eval_pdf <- try(WienR::WienerPDF(t = rt, response = response, a = a,
                                   v = v, w = w, t0 = t0, sv = sv, sw = sw,
                                   st0 = st0, ...),
                  silent = TRUE)

  if (any(class(eval_pdf) == "try-error")) return(rep(NaN, length(par)))

  # Derivative of log(f(x)) is f'(x) / f(x)

  grad <- rep(NaN, length(par))
  names(grad) <- names(par)

  for (i in 1:max(bound_index)) {
    grad[paste0("a[", i, "]")] <- sum(eval_grad$deriv[bound_index == i, "da"] /
                                        eval_pdf$value[bound_index == i])
    grad[paste0("w[", i, "]")] <- sum(eval_grad$deriv[bound_index == i, "dw"] /
                                        eval_pdf$value[bound_index == i])
  }

  if (use_sw) {
    for (i in 1:max(sw_index)) {
      grad[paste0("sw[", i, "]")] <- sum(eval_grad$deriv[sw_index == i, "dsw"] /
                                           eval_pdf$value[sw_index == i])
    }
  }

  for (i in 1:max(drift_index)) {
    grad[paste0("v[", i, "]")] <- sum(eval_grad$deriv[drift_index == i, "dv"] /
                                        eval_pdf$value[drift_index == i])
  }

  if (use_sv) {
    for (i in 1:max(sv_index)) {
      grad[paste0("sv[", i, "]")] <- sum(eval_grad$deriv[sv_index == i, "dsv"] /
                                           eval_pdf$value[sv_index == i])
    }
  }

  for (i in 1:max(resid_index)) {
    grad[paste0("t0[", i, "]")] <-
      sum(eval_grad$deriv[resid_index == i, "dt0"] /
            eval_pdf$value[resid_index == i])
  }

  if (use_st0) {
    for (i in 1:max(st0_index)) {
      grad[paste0("st0[", i, "]")] <-
        sum(eval_grad$deriv[st0_index == i, "dst0"] /
              eval_pdf$value[st0_index == i])
    }
  }

  return(-grad)
}



#' Find The Negative Log Likelihood of a Parameter
#'
#' A helper function used by `'fit_wienr()` to find the negative log likelihood
#' of a set of parameters
#'
#' @inheritParams gradient
#' @param ... Optional arguments to pass to `'WienerPDF()'`.
#'
#' @returns Something
#'
#' @export
#'
#' @examples
#' \dontrun{
#' nll()
#' }

nll <- function(par, rt, response, bound_index, drift_index, resid_index,
                sv_index = NULL, sw_index = NULL, st0_index = NULL, ...) {

  a <- par[paste0("a[", bound_index, "]")]
  v <- par[paste0("v[", drift_index, "]")]
  w <- par[paste0("w[", bound_index, "]")]
  t0 <- par[paste0("t0[", resid_index, "]")]

  if (is.na(par["sv[1]"])) sv <- 0 else sv <- par[paste0("sv[", sv_index, "]")]

  if (is.na(par["sw[1]"])) sw <- 0 else sw <- par[paste0("sw[", sw_index, "]")]

  if (is.na(par["st0[1]"])) st0 <- 0 else st0 <- par[paste0("st0[", st0_index, "]")]

  eval_pdf <- try(WienR::WienerPDF(t = rt, response = response, a = a,
                                   v = v, w = w, t0 = t0, sv = sv, sw = sw,
                                   st0 = st0, ...),
                  silent = TRUE)

  if (any(class(eval_pdf) == "try-error")) return(Inf)

  return(-sum(eval_pdf$logvalue))
}



#' Title
#'
#' @param p Start
#' @param response Start
#' @param ... Optional arguments to pass to the functions from the `'WienR'`
#'  package.
#'
#' @returns Something
#'
#' @export
#'
#' @examples
#' \dontrun{
#' q_wdm()
#' }

q_wdm <- function(p, response, ...) {

  p_resp <- WienR::WienerCDF(Inf, response = response, ...)$value

  res <- try(
    uniroot(f = function(t) {
      WienR::WienerCDF(t, response = response, ...)$value} / p_resp - p,
      interval = c(0, 5),
      f.lower = -p,
      extendInt = "upX"
    )
  )

  if (class(res) == "try-error") NA else res$root #You may need to add back in
  # the explicit return() here.

}



#' File Reader
#'
#' A helper used by `'read_file_list()'` to read in individual data files.
#'
#' @param x A filepath in the form of a string.
#'
#' @returns A `data.table` object containing data from a file supplied to
#' `'read_data_list()'`.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' info <- files_info()
#' file <- read_file(x = info$filepath)
#' }

read_file <- function(x) {

  data.table::fread(
    file = x,
    na.strings = c("", "null", NA),
    nThread = data.table::getDTthreads(),
    data.table = TRUE
  )

}



#' A Reader for Lists of Files
#'
#' A wrapper to read in multiple files passed from the `'read_file()'` helper
#' and bind them together into a single object.
#'
#' @param files A string or an object containing filepath string(s).
#'
#' @returns A `data.table` object with concatenated data from all named files
#'  supplied in `'file_list'`.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' info <- files_info()
#' data <- read_file_list(files = info$filepath)
#' }

read_file_list <- function(files) {

  for (i in seq_along(files)) {

    list <- list(read_file(x = files[i]))

  }

  data.table::rbindlist(list)

}



#' Make Consistently Themed Plots
#'
#' A helper function to set the aesthetics of plots to ensure plots formatted
#' with the `'theme_pcj()'` function all have the same settings.
#'
#' @param base_size The plot's default font size; must be numeric.
#' @param dark_text A quoted hex code that sets the color of the darkest text in
#' the plot. All text is based on shades of the specified hex code.
#' @param font A character string containing the name of the font in which to
#' print the plot's text.
#' @param ... Optional arguments to be passed to `'theme()'`.
#'
#' @import ggplot2
#'
#' @keywords internal
#'
#' @returns A plot configured with the declared aesthetics.
#'
#' @examples
#' \dontrun{
#' ggplot2::ggplot(data = mtcars, ggplot2::aes(x = wt, y = mpg, color = gear)) +
#' ggplot2::geom_point() +
#' theme_pcj_aesthetics(
#' base_size = 12,
#' dark_text = "#000000",
#' font = "Atkinson Hyperlegible")
#' }

theme_pcj_aesthetics <- function(base_size,
                                 dark_text,
                                 font,
                                 ...) {

  mid_text <- monochromeR::generate_palette(
    colour = dark_text,
    modification = "go_lighter",
    n_colours = 9
  )[4]

  light_text <- monochromeR::generate_palette(
    colour = dark_text,
    modification = "go_lighter",
    n_colours = 9
  )[7]

  if (!font %in% systemfonts::system_fonts()$family) {
    font <- "sans"  # fallback to system font
  }

  theme_minimal() %+replace%
    theme(
      text = element_text(
        family = font,
        size = base_size,
        face = "bold",
        lineheight = 1.1,
        margin = margin(t = 0, r = 0, b = 10, l = 0)
      ),
      plot.title = element_text(
        color = dark_text,
        size = rel(2),
        hjust = 0
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.subtitle = element_text(
        size = rel(1.5),
        color = mid_text,
        hjust = 0
      ),
      axis.text = element_text(
        size = rel(1.0),
        color = dark_text
      ),
      axis.title = element_text(
        size = rel(1.2),
        color = dark_text
      ),
      panel.grid = element_line(
        color = mid_text,
        linewidth = .15,
        linetype = "dashed"
      ),
      plot.caption = element_text(
        color = light_text,
        hjust = 1
      ),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.text = element_text(
        size = rel(1.2),
        lineheight = 1,
        color = mid_text
      ),
      legend.direction = "horizontal",
      ...
    )

}



#' Plot Color Palettes
#'
#' A helper function defining the palettes accessible to `'theme_pcj()'`.
#'
#' @param palette A string. The name of the palette to be mapped to a variable,
#' including: "default", "neg_to_pos", "mono_printing", "mono_blue", "mono_red",
#' "mono_yellow", or "ualbany".
#' @param continuous Logical. Is the variable continuous or discrete?
#' @param .colors Loads the colors defined within pcj_colors.
#' @param .palettes Loads the palettes defined within pcj_palettes.
#'
#' @keywords internal
#'
#' @returns A plot object with the specified aesthetics rendered.
#'
#' @examples
#' \dontrun{
#' ggplot2::ggplot(
#' data = mtcars,
#' mapping = ggplot2::aes(x = wt, y = mpg, color = gear)) +
#' ggplot2::geom_point() +
#' pcj_graph_palettes(
#' palette = "ualbany",
#' continuous = FALSE)
#' }

theme_pcj_palettes <- function(palette, continuous,
                               .colors = pcj_colors,
                               .palettes = pcj_palettes) {
  # Define colors
  pcj_colors <- list(
    black = "#000000",
    cyan = "#88CCEE",
    dark_blue = "#004488",
    dark_red = "#994455",
    dark_yellow = "#997700",
    green = "#117733",
    indigo = "#332288",
    teal = "#44AA99",
    light_blue = "#6699CC",
    light_red = "#EE99AA",
    light_yellow = "#EECC66",
    olive = "#999933",
    pale_gray = "#DDDDDD",
    purple = "#AA4499",
    rose = "#CC6677",
    sand = "#DDCC77",
    white = "#FFFFFF",
    wine = "#882255",
    gold = "#eeb211",
    purple_dark = "#46166b",
    gray = "#a2aaad"
  )

  # Combine colors into different palettes
  pcj_palettes <- list(
    default = c(
      pcj_colors$cyan,
      pcj_colors$green,
      pcj_colors$indigo,
      pcj_colors$teal,
      pcj_colors$olive,
      pcj_colors$pale_gray,
      pcj_colors$purple,
      pcj_colors$rose,
      pcj_colors$sand,
      pcj_colors$wine
    ),
    neg_to_pos = c(
      pcj_colors$wine,
      pcj_colors$sand
    ),
    mono_printing = c(
      pcj_colors$white,
      pcj_colors$light_yellow,
      pcj_colors$light_red,
      pcj_colors$light_blue,
      pcj_colors$dark_yellow,
      pcj_colors$dark_red,
      pcj_colors$dark_blue,
      pcj_colors$black
    ),
    mono_blue = c(
      pcj_colors$light_blue,
      pcj_colors$dark_blue
    ),
    mono_red = c(
      pcj_colors$light_red,
      pcj_colors$dark_red
    ),
    mono_yellow = c(
      pcj_colors$light_yellow,
      pcj_colors$dark_yellow
    ),
    ualbany = c(
      pcj_colors$gold,
      pcj_colors$purple_dark,
      pcj_colors$black,
      pcj_colors$gray
    )
  )

  if (continuous == FALSE) {

    ggplot2::discrete_scale(
      palette = grDevices::colorRampPalette(.palettes[[palette]]),
      aesthetics = c("color", "fill"),
      na.value = .colors$na_value
    )

  } else {

    ggplot2::scale_color_gradientn(
      colors = .palettes[[palette]],
      na.value = .colors$na_value
    )

  }
}



#' Plot Text Settings
#'
#' A helper to define the text for various elements of a plot as part of the
#' `'theme_pcj()'` function.
#'
#' @param plot_text A named character vector where plot features are names and
#'  the text to be printed in the plot are values (e.g.,
#'  c(title = "Plot Title", etc.)).
#' @param alt_text Logical. Should a subtitle and caption be generated for the
#'  plot?
#'
#' @keywords internal
#'
#' @returns A ggplot object with the text declared in the function call.
#'
#' @examples
#' \dontrun{
#' ggplot2::ggplot(data = mtcars, ggplot2::aes(x = wt, y = mpg, color = gear)) +
#'   ggplot2::geom_point() +
#'   theme_pcj_text()
#'   }

theme_pcj_text <- function(plot_text, alt_text) {

  # In case the full expected names of the plot elements aren't given
  names(plot_text) <- match.arg(arg = names(plot_text),
                                choices = c("title", "subtitle",
                                            "xlab", "ylab", "caption"),
                                several.ok = TRUE)

  default_caption <- paste0("Created: ", format(Sys.time(), "%Y%m%d, %H:%M"))

  if (alt_text == TRUE) {

    labs(
      title = if ("title" %in% names(plot_text))
        plot_text[["title"]] else "title",
      subtitle = if ("subtitle" %in% names(plot_text))
        plot_text[["subtitle"]] else "subtitle",
      x = if ("xlab" %in% names(plot_text))
        plot_text[["xlab"]] else "xlab",
      y = if ("ylab" %in% names(plot_text))
        plot_text[["ylab"]] else "ylab",
      caption = if ("caption" %in% names(plot_text))
        plot_text[["caption"]] else default_caption
    )

  } else {

    labs(
      title = if ("title" %in% names(plot_text))
        plot_text[["title"]] else "title",
      subtitle = NULL,
      x = if ("xlab" %in% names(plot_text))
        plot_text[["xlab"]] else "xlab",
      y = if ("ylab" %in% names(plot_text))
        plot_text[["ylab"]] else "ylab",
      caption = NULL
    )
  }
}
