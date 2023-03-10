% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fql_wrapper.R
\name{fql_wrapper}
\alias{fql_wrapper}
\title{wrapper function to pipe non standard arguments to fql style select/update. Should not be used directly}
\usage{
fql_wrapper(d, ..., where, by, fql_fun, exclude = NULL, env = NULL)
}
\arguments{
\item{d}{data.frame}

\item{...}{'what' arguments: a collection of column names or column definitions}

\item{where}{boolean condition on columns, under the form cond1 if only one condition, and c(cond1, cond2, ...) if more than one condition}

\item{by}{collection of columns, under the form col1 if only one condition, and c(col1, col2, ...) if more than one column}

\item{fql_fun}{the fql_fun to apply to d}

\item{exclude}{a character vector containing columns to exclude}
}
\value{
the output data.frame
}
\description{
wrapper function to pipe non standard arguments to fql style select/update. Should not be used directly
}
