% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drift_mod.R
\name{drift_mod}
\alias{drift_mod}
\title{A Drift Diffusion Model}
\usage{
drift_mod(
  nsims = 1000,
  v = 0.5,
  sv = 0,
  a = 2,
  w = 0.5,
  sw = 0.9,
  t0 = 0,
  st0 = 0.2,
  dt = 0.01,
  t_max = Inf,
  plots = TRUE,
  ...
)
}
\arguments{
\item{nsims}{The number of simulation pf the drift difussion model to run}

\item{v}{The average drift rate towards a boundary.}

\item{sv}{Drift rate standard deviation: variability of the sampled drift
rate. When multiple simulations are run, such as with the \code{'drift_mod()'}
function, this allows drift rates to change from one simulation to the next.}

\item{a}{Response caution: the threshold to come to a decision represented
by the distance between the starting point and the}

\item{w}{Response bias mean: the avg ease of traveling to either boundary}

\item{sw}{Boundary/starting point deviation}

\item{t0}{Residual time: time lag between evidence accumulation and action}

\item{st0}{The mean of the residual time distribution}

\item{dt}{Delta t: the units by which time increments}

\item{t_max}{Maximum time: the max time the model will draw samples}

\item{plots}{Logical: should plot elements be generated?}

\item{...}{optional arguments to be passed to \code{'theme_pcj()'}.}
}
\value{
A list, including the results of the simulation, and plots for the
average evidence accumulation over time, the conditional rt distribution,
and the quantile-probability plot
}
\description{
A Drift Diffusion Model
}
\examples{
drift_mod()
}
