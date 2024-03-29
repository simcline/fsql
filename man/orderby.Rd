% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orderby.R
\name{orderby}
\alias{orderby}
\title{Similar to SQL order by function}
\usage{
orderby(d, ...)
}
\arguments{
\item{d}{data.frame}

\item{...}{columns to order. Columns in descending order should be prefixed by "-".}
}
\value{
the ordered data.frame
}
\description{
Similar to SQL order by function
}
\examples{
d <- data.frame(
date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
ret1=rnorm(10),
ret2=rnorm(10),
ret3=rnorm(10)
)

d \%>\% orderby(ret1)
d \%>\% orderby(ticker, -ret1)
}
