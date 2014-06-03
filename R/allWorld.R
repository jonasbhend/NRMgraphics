#' @name allWorld
#' @aliases allWorldKey
#' 
#' @title
#' Giorgi regions overview plots
#' 
#' @description
#' This function provides the backbone for the global overview plots.
#' In essence, it provides the map and the locations where to add insets.
#' Also \code{allWorldKey} offers the additional functionality of 
#' placing a legend in the bottomleft corner.
#' 
#' @param fun function to be executed at the respective spots for insets
#' @param add.legend logical, should legend for map background be added?
#' @param regnames which regions should be plotted (see details)
#' @param inset.bg background colour for insets (with transparency)
#' @param ... additional arguments passed to \code{fun}
#' 
#' @details
#' The list for background colouring has to contain at least the object 
#' \code{data}, a vector with values to use in colouring named with the region
#' abbreviations. In addition, \code{col} and \code{lev} can be added to control
#' the colouring. \code{title} will add a title and \code{units} 
#' will add units to the legend.
#' 
#' @keywords plot
#' @export
allWorld <- function(fun, regnames=NULL, inset.bg='#FFFFFFCC', ...){
  ## set the map dimensions
  mapdim <- c(-180, 180, -60, 85)
  lons <- c(GLO=-160, AUS=132, AMZ=-58, SSA=-58, CAM=-100, WNA=-140, CNA=-100, ENA=-60, ALA=-140, GRL=-40, MED=15, NEU=15, WAF=-5, EAF=35, SAF=21, SAH=15, SEA=125, EAS=135, SAS=82, CAS=55, TIB=95, NAS=110, SAU=132, NAU=132, SEAUS=148, SWAUS=117, NWAUS=122, TNAFR=-5, TINDO=75, TTIBE=105, NHILA=0, SHILA=0, TROLA=65, TROAF=20, TROIN=130, TGLO=-160, OCEAN=-160, NHMEA=0, SHMEA=0, MER1=0, MER2=0)
  lats <- c(GLO=-22, AUS=-28, AMZ=-4, SSA=-38, CAM=20, WNA=43, CNA=43, ENA=43, ALA=66, GRL=70, MED=40, NEU=62, WAF=-4, EAF=-4, SAF=-26, SAH=18, SEA=4, EAS=35, SAS=18, CAS=40, TIB=40, NAS=62, SAU=-40, NAU=-18, SEAUS=-37, SWAUS=-34, NWAUS=-18, TNAFR=32, TINDO=-2, TTIBE=45, NHILA=70, SHILA=-70, TROLA=0, TROAF=0, TROIN=0, TGLO=0, OCEAN=-44, NHMEA=45, SHMEA=-45, MER1=58, MER2=28)
  ##plot(0, type='n', xlim=mapdim[1:2], ylim=mapdim[3:4], asp=1/cos(mean(mapdim[3:4])/180*pi), axes=F, xlab='', ylab='', xaxs='i', yaxs='i')
  plot(0, type='n', xlab='', ylab='', xlim=mapdim[1:2], ylim=mapdim[3:4], axes=F)
  map(interior=F, add=T, col='lightgrey', fill=TRUE, border='darkgrey', xlim=par('usr')[1:2], ylim=par('usr')[3:4])
  ## plot background
  if (is.null(regnames)) regnames <- names(lons)[c(1,3:24, 36, 37)]
  ## loop over regions and apply main function
  if (all(!is.na(regnames))){
    size <- c(0.1, 0.13)
    for (regn in regnames){
      inset(inset.x=lons[regn], inset.y=lats[regn], inset.size=size, inset.bg=inset.bg, fun=fun, regn=regn, ...=...)
    }
  }
  ## plot bounding box
  box()
}

#' @rdname allAustralia
#' @export
allWorldKey <- function(fun, ...){
  inset(inset.x=mapdim[1]+0.1*diff(mapdim[1:2]), inset.y=mapdim[3] + 0.1*diff(mapdim[3:4]), inset.size=c(0.1, 0.15), fun=fun, ...=...)
}
