% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trim_values.R
\name{trim_values}
\alias{trim_values}
\title{Replace out-of-range values}
\usage{
trim_values(x, below = -Inf, above = Inf, replace_with = NA)
}
\description{
Replace out-of-range values
}
\examples{
x <- rnorm(n = 10, mean = 0, sd = 1.0)
trim_values(x, below = -0.1, above = 0.1, replace_with = NA)

}
