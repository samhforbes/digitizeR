% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_bad.R
\name{adjust_points}
\alias{adjust_points}
\title{Adjust the points so they have the right number}
\usage{
adjust_points(data, npoints)
}
\arguments{
\item{data}{the dataset to adjust}

\item{npoints}{The correct number of points it should have, but currently doesn't}
}
\value{
A set of caps with the correct number of points (although some points will be bad)
}
\description{
This is just necessary to take bad templates further along the pipeline.
}
