% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{theme_pcj_aesthetics}
\alias{theme_pcj_aesthetics}
\title{Make Consistently Themed Plots}
\usage{
theme_pcj_aesthetics(base_size, dark_text, font, ...)
}
\arguments{
\item{base_size}{The plot's default font size; must be numeric.}

\item{dark_text}{A quoted hex code that sets the color of the darkest text in
the plot. All text is based on shades of the specified hex code.}

\item{font}{A character string containing the name of the font in which to
print the plot's text.}

\item{...}{Optional arguments to be passed to \code{'theme()'}.}
}
\value{
A plot configured with the declared aesthetics.
}
\description{
A helper function to set the aesthetics of plots to ensure plots formatted
with the \code{'theme_pcj()'} function all have the same settings.
}
\examples{
\dontrun{
ggplot2::ggplot(data = mtcars, ggplot2::aes(x = wt, y = mpg, color = gear)) +
ggplot2::geom_point() +
theme_pcj_aesthetics(
base_size = 12,
dark_text = "#000000",
font = "Atkinson Hyperlegible")
}
}
\keyword{internal}
