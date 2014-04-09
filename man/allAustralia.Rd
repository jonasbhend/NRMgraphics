\name{allAustralia}
\alias{allAustralia}
\alias{allAustraliaKey}
\title{Australian overview plots}
\usage{
allAustralia(fun, bg = NULL, add.legend = TRUE, old = FALSE,
  regnames = NULL, ...)

allAustraliaKey(fun, ...)
}
\arguments{
  \item{fun}{function to be executed at the respective
  spots for insets}

  \item{bg}{list containing information to colour the map
  background (see details)}

  \item{add.legend}{logical, should legend for map
  background be added?}

  \item{old}{logical, should old regions be used (RE,RW
  instead of RN,RS)?}

  \item{regnames}{which regions should be plotted (see
  details)}

  \item{...}{additional arguments passed to \code{fun}}
}
\description{
This function provides the backbone for the Australian
overview plots. In essence, it provides the map and the
locations where to add insets. Also \code{allAustraliaKey}
offers the additional functionality of placing a legend in
the bottomleft corner.
}
\details{
The list for background colouring has to contain at least
the object \code{data}, a vector with values to use in
colouring named with the region abbreviations. In addition,
\code{col} and \code{lev} can be added to control the
colouring. \code{title} will add a title and \code{units}
will add units to the legend.
}
\keyword{plot}

