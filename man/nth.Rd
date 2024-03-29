% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{nth}
\alias{nth}
\title{Pick nth element}
\usage{
nth(x, n)
}
\arguments{
\item{x}{any structure compatible with simple-bracket slicing}

\item{n}{index}
}
\value{
element in position n of x
}
\description{
Pick nth element
}
\examples{
x <- c(1,2,3)
nth(x,2)
nth(x,-1)
}
