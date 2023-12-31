% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ntickets.R
\name{ntickets}
\alias{ntickets}
\title{Function that calculates the number of tickets to be sold when the number of seats in the flight is N and the
probability of a "show" is p and gamma is the probability a plane will be truly overbooked}
\usage{
ntickets(N = 200, gamma = 0.02, p = 0.95)
}
\arguments{
\item{N}{the maximum capacity}

\item{gamma}{the maximum acceptable risk of failure.}

\item{p}{target the probability of success}
}
\value{
returns a list
}
\description{
Function that calculates the number of tickets to be sold when the number of seats in the flight is N and the
probability of a "show" is p and gamma is the probability a plane will be truly overbooked
}
\examples{
ntickets(N = 200, gamma = 0.02, p = 0.95)

}
