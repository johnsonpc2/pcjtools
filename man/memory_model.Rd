% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/memory_model.R
\name{memory_model}
\alias{memory_model}
\title{A function that runs a simulation of an
Evidence-Based Random Walk memory model}
\usage{
memory_model(
  seed = 20240709,
  n_letters = 12,
  n_trials = 500,
  updated_probs_sd = 0.1,
  max_forgetting = 0.6,
  forgetting_slope = 3,
  initial_exposure_matrix = NULL
)
}
\arguments{
\item{seed}{numeric. A single integer to generate consistent random numbers.}

\item{n_letters}{an integer. The number of letters used in the model.}

\item{n_trials}{an integer. The number of iterations the model runs.}

\item{updated_probs_sd}{an integer. The standard deviation of random samples
drawn to update probabilities.}

\item{max_forgetting}{a single integer, the probability of forgetting any one
item after you've learned it.}

\item{forgetting_slope}{a single integer, the rate you need to see an item to
decrease the probability it will be forgotten.}

\item{initial_exposure_matrix}{optional. A matrix of initial letter exposures
to be used to start the model running. if NULL an initial exposure matrix
will be generated from scratch.}
}
\value{
a matrix of learned memory strengths between letters, a matrix of the
randomly generated probabilities used when drawing random samples of letters,
and three graphs of the strengths between letters in memory the model's
memory representation.
}
\description{
A function that runs a simulation of an
Evidence-Based Random Walk memory model
}
\examples{
memory_model(seed = 1234, n_letters = 12, n_trials = 200)
}
