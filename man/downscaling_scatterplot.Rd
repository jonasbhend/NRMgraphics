\name{downscaling_scatterplot}
\alias{downscaling_scatterplot}
\title{Scatterplot of downscaled vs. GCM data}
\usage{
downscaling_scatterplot(df, xlim = range(df, na.rm = T), ylim = range(df,
  na.rm = T), add.names = FALSE, correlation = FALSE, ...)
}
\arguments{
  \item{df}{list of data.frames with model data (see
  details)}

  \item{xlim,ylim}{horizontal and vertical extent of plot}

  \item{add.names}{logical, should model names be added?}

  \item{correlation}{logical, show correlation instead of
  regression?}

  \item{...}{additional arguments passed to
  \code{\link{plot}}}
}
\description{
Plots a scattter plot of downscaled vs. GCM data with
corresponding regression line (or correlation) for
different scenarios.
}
\details{
The data frames (one for each scenario) are expected to
contain elements named Downscaled and GCM. In addition, for
\code{add.names} to work, the rows need to contain the
model names.
}
\keyword{plot}

