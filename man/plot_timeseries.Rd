\name{plot_timeseries}
\alias{plot_timeseries}
\title{Plot time series (plume)}
\usage{
plot_timeseries(dquant, dfiltquant, omn = 0, obs = NULL, modts = NULL,
  modi = 1, baselining = NULL, seas = "", xlim = NULL, ylim = NULL,
  add = FALSE, relative = FALSE, startyear = 1901, endyear = 2099,
  las = 2, ylab = "", scenario = "rcp85", xaxt = "s", yaxt = "s",
  add.legend = TRUE, as.lines = FALSE, add.grid = FALSE)
}
\arguments{
  \item{dquant}{list of (outer) quantile ranges for
  different scenarios}

  \item{dfiltquant}{list of (inner) quantile ranges}

  \item{omn}{Climatology to be added to everything
  (multiplied if \code{relative})}

  \item{obs}{list of observations to be added as lines (one
  each)}

  \item{modts}{list of example model data to be overlaid}

  \item{modi}{index of model(s) to be overlaid}

  \item{baselining}{boundaries of shading to indicate
  reference period}

  \item{seas}{Text to be included as title (topleft inside
  graph)}

  \item{xlim,ylim}{extent of plot}

  \item{add}{logical, whether plot should be added to
  existing plot}

  \item{relative}{logical, climatology is multiplied if
  \code{TRUE}}

  \item{startyear,endyear}{range of years to which plot is
  clipped}

  \item{las}{orientation of axis labels (see \code{par})}

  \item{ylab}{labels for vertical axis}

  \item{scenario}{scenario to be plotted}

  \item{xaxt,yaxt}{character descriping x and y axis type}

  \item{add.legend}{logical, should legend be added?}

  \item{as.lines}{logical, should individual model time
  series be shown?}

  \item{add.grid}{logical, should horizontal grid lines be
  added?}
}
\value{
vertical limits of plots (e.g. \code{ylim})
}
\description{
Function to plot timeseries given objects of class 'NetCDF'
}
\keyword{plot}

