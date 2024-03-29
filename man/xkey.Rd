% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xkey.R
\name{xkey}
\alias{xkey}
\title{Sets the primary keys in a table.}
\usage{
xkey(d, keys)
}
\arguments{
\item{d}{data.frame}

\item{keys}{char vector listing the key columns}
}
\value{
data.frame with an additional key attribute
}
\description{
Sets the primary keys in a table.
}
\examples{

d <- data.frame(
date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
ret1=rnorm(10),
ret2=rnorm(10),
ret3=rnorm(10)
)

a <- d \%>\% xkey("ticker")
attributes(a)$key

}
