#' Graphics and analysis for the NRM projections project
#'
#' Package for plotting of CMIP5 and other climate model output
#' 
#' @import maps mapdata geoutils 
#' @docType package
#' @name NRMgraphics
NULL

#' NRM region boundaries
#' 
#' A dataset containing the region boundaries for the NRM clusters and sub-
#' clusters. 
#' 
#' @docType data
#' @keywords datasets
#' @format A list of lists with lon and lat values (x and y)
#' @name nrmregions
NULL

#' NRM test data for plots
#' 
#' A dataset containing simulated and observed temperature time
#' series and temperature change percentiles for examples.
#' 
#' @docType data
#' @keywords datasets
#' @format lists of NetCDF objects
#' @name nrmexample
NULL

#' NRM colours
#' 
#' A dataset containing the colours used in the NRM report.
#' This dataset includes:
#'   \describe{
#'     \item{scencols}{Scenario colours used for bars and inner shaded area}
#'     \item{scencols2}{Scenario colours for median value (darker)}
#'     \item{scencols3}{Scenario colours for outer shaded area (lighter)}
#'     \item{modcol,modcol2}{Colour for model overlays}
#'     \item{obscol}{Colour for observations}
#'     \item{blackcol}{Colour for axes and text}
#'     \item{heatcols,heatcols2}{Dark and light shades of colours for positive 
#'           and negative values (red-blue)}
#'     \item{wetcols,wetcols2}{Dark and light shades for rainfall related 
#'           variables (blue-brown)}
#'   }
#' 
#' @docType data
#' @keywords datasets
#' @format Named vectors with hex colour codes.
#' @aliases scencols scencols2 scencols3 modcol modcol2 blackcol
#'     heatcols heatcols2 obscol oldcolours scencolvals wetcols wetcols2
#' @name NRMcolours
NULL

#' NRM scenario names
#' 
#' A vector of long-form names for the different RCPs
#' 
#' @docType data
#' @keywords datasets
#' @format A named character vector
#' @name scennames
NULL

#' NRM season names
#' 
#' A vector of long-form names for the seasons
#' 
#' @docType data
#' @keywords datasets
#' @format A named character vector
#' @name seastxt
NULL

#' NRM region names
#' 
#' A vector of long-form names for the different NRM clusters and subclusters
#' 
#' @docType data
#' @keywords datasets
#' @format A named character vector
#' @name regtxt
NULL

