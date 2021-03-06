% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ROC_sub2.R
\name{makeCoord}
\alias{makeCoord}
\title{Calculate x, y coordinate for ROC curve}
\usage{
makeCoord(x, no = 1)
}
\arguments{
\item{x}{A list As a result of Epi::ROC()}

\item{no}{Integer}
}
\value{
A data.frame
}
\description{
Calculate x, y coordinate for ROC curve
}
\examples{
require(Epi)
x=ROC(form=am~wt,data=mtcars,plot="ROC")
makeCoord(x)
}
