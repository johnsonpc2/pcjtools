% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_data.R
\name{import_data}
\alias{import_data}
\title{Data Importer}
\usage{
import_data(x)
}
\arguments{
\item{x}{A string or list of strings—e.g., c(string1, string2)—containing the
path(s) of file(s) to read in, or can be the name of a \code{data.table} or
\code{dataframe} column in the form \code{dt$filepath_col} which contains filepaths.
All files in a list will be read in and returned as a single \code{data.table}
object.}
}
\value{
A \code{data.table} object containing the read-in data from the paths
specified in \code{x}.
}
\description{
A function to import data files from a local directory. Designed
to read a list of file path strings, principally, those stored in the
first column of the \code{data.table} produced by the \code{file_info()} function from
\code{'pcjtools'}.
}
\examples{
info <- files_info()
file <- import_data(x = info$filepath)
}
