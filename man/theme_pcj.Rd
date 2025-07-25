% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_pcj.R
\name{theme_pcj}
\alias{theme_pcj}
\title{Custom Theme}
\usage{
theme_pcj(
  plot,
  base_size = 12,
  dark_text = "#000000",
  font = "Atkinson Hyperlegible",
  palette = "default",
  continuous = FALSE,
  plot_text = c(title = "title", subtitle = "subtitle", xlab = "xlab", ylab = "ylab",
    caption = paste0("Created: ", format(Sys.time(), "\%Y\%m\%d, \%H:\%M"))),
  alt_text = TRUE,
  save_path = NULL,
  ...
)
}
\arguments{
\item{plot}{A ggplot object to be formatted.}

\item{base_size}{The plot's default font size; must be numeric.}

\item{dark_text}{A quoted hex code that sets the color of the darkest text in
the plot. All text is based on shades of the specified hex code.}

\item{font}{A character string containing the name of the font in which to
print the plot's text.}

\item{palette}{A string. The name of the palette to be mapped to a variable,
including: "default", "neg_to_pos", "mono_printing", "mono_blue", "mono_red",
"mono_yellow", or "ualbany".}

\item{continuous}{Logical. Is the variable continuous or discrete?}

\item{plot_text}{A named character vector where plot features are names and
the text to be printed in the plot are values (e.g.,
c(title = "Plot Title", etc.)).}

\item{alt_text}{Logical. Should a subtitle and caption be generated for the
plot?}

\item{save_path}{A string. The directory path where the plot should be saved.}

\item{...}{Additional arguments passed to \code{theme()} or \code{ggsave()}.}
}
\value{
A plot object with the specified aesthetics.
}
\description{
A function to create and save consistent plots with aesthetics chosen to
enhance accessibility
}
\examples{
g1 <- ggplot2::ggplot(
data = mtcars,
ggplot2::aes(x = mpg, y = wt, color = factor(cyl))
) +
ggplot2::geom_line(linewidth = 2)

thm <- theme_pcj(plot = g1, font = "sans")
}
