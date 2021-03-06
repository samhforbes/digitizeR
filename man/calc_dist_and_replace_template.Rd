% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_3d_dist.R
\name{calc_dist_and_replace_template}
\alias{calc_dist_and_replace_template}
\title{Calculate the 3d distance and replace points outside with the template}
\usage{
calc_dist_and_replace_template(template, data, max)
}
\arguments{
\item{template}{the template data to replace with}

\item{data}{the data to replace}

\item{max}{the maximum distance}
}
\value{
data where bad points are replaced with the template
}
\description{
Calculate the 3d distance and replace points outside with the template
}
