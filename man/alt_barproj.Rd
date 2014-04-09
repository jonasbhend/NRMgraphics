\name{alt_barproj}
\alias{alt_barproj}
\title{Plot grouped projection bars with inner shading}
\usage{
alt_barproj(meanchange, meansd = NULL, ylim = NULL, col = hcl(c(240, 0), l
  = 60, c = 50), col2 = hcl(c(240, 0), l = 30, c = 80), lwd = 10,
  siglevel = 0.2, lty = 1, distance = 0.5)
}
\arguments{
  \item{meanchange}{list of percentile changes}

  \item{meansd}{list of standard dev. or percentile changes
  for nat. var.}

  \item{ylim}{vertical extent of plot}

  \item{col,col2}{colours for positive and negative
  segments of bars}

  \item{lwd}{bar width}

  \item{siglevel}{significance level to convert sdev. to
  shading ranges}

  \item{lty}{line type for horizontal grid lines}

  \item{distance}{relative distance between group of bars}
}
\description{
Bar plots grouped by scneario or seas with inner shading
reflecting natural variability.
}
\keyword{plot}

