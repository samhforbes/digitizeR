% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modified_kabsch.R
\name{modified_kabsch}
\alias{modified_kabsch}
\title{modified version of the kabsch algorithm}
\usage{
modified_kabsch(pm, qm, om)
}
\arguments{
\item{pm}{a matrix}

\item{qm}{a matrix}

\item{om}{a third matrix}
}
\value{
om transposed.
}
\description{
adapted to allow application of the matrix to a third dataset. Adapted from the kabsch function used in this package, original site listed there.
}
