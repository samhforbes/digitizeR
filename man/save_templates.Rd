% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_caps.R
\name{save_templates}
\alias{save_templates}
\title{Save template output}
\usage{
save_templates(template, data, path)
}
\arguments{
\item{template}{list of template caps}

\item{data}{the dataset aligned to the templates - for naming purposes only}

\item{path}{the path to save the digpts.txt files}
}
\value{
a directory of folders named with the cap size, each of which contains digpts.txt
}
\description{
This function is experimental and will write the caps as digpts.txt to disk.
Note that the type argument would need adjustment for your cap geometry, the saving function also multipleis by 10 to go to mm for AtlasViewerGUI
}
