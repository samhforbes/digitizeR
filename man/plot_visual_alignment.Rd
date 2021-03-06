% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_one_cap.R
\name{plot_visual_alignment}
\alias{plot_visual_alignment}
\title{Plot to see the alignment of landmarks for each cap size}
\usage{
plot_visual_alignment(template_index, nested_data, flipped = T)
}
\arguments{
\item{template_index}{the results of find_greatest_alignment}

\item{nested_data}{the resutls of align_all_caps_nested}

\item{flipped}{logical, if true will diplay cap right way up}
}
\value{
a plot of the landmarks for each cap size
}
\description{
This is a way to check that all capsize lists are roughly in the same space
}
