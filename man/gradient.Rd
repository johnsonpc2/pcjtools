% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{gradient}
\alias{gradient}
\title{Title}
\usage{
gradient(
  par,
  rt,
  response,
  bound_index,
  drift_index,
  resid_index,
  sv_index = NULL,
  sw_index = NULL,
  st0_index = NULL,
  ...
)
}
\arguments{
\item{par}{Start}

\item{rt}{Start}

\item{response}{Start}

\item{bound_index}{Start}

\item{drift_index}{Start}

\item{resid_index}{Start}

\item{sv_index}{Start}

\item{sw_index}{Start}

\item{st0_index}{Start}

\item{...}{Optional arguments to pass to \code{'WienR'} package functions.}
}
\value{
A vector of gradients
}
\description{
Title
}
\examples{
gradient(par, rt, response, bound_index, drift_index, resid_index)
}
