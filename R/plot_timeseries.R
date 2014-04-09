#' Plot time series (plume)
#'
#' Function to plot timeseries given objects of class 'NetCDF'
#'
#' @param dquant list of (outer) quantile ranges for different scenarios
#' @param dfiltquant list of (inner) quantile ranges
#' @param omn Climatology to be added to everything (multiplied if \code{relative})
#' @param obs list of observations to be added as lines (one each)
#' @param modts list of example model data to be overlaid
#' @param modi index of model(s) to be overlaid
#' @param baselining boundaries of shading to indicate reference period
#' @param seas Text to be included as title (topleft inside graph)
#' @param xlim,ylim extent of plot
#' @param add logical, whether plot should be added to existing plot
#' @param relative logical, climatology is multiplied if \code{TRUE}
#' @param startyear,endyear range of years to which plot is clipped
#' @param las orientation of axis labels (see \code{par})
#' @param ylab labels for vertical axis
#' @param scenario scenario to be plotted
#' @param xaxt,yaxt character descriping x and y axis type
#' @param add.legend logical, should legend be added?
#' @param as.lines logical, should individual model time series be shown?
#' @param add.grid logical, should horizontal grid lines be added?
#' 
#' @return vertical limits of plots (e.g. \code{ylim})
#' 
#' @keywords plot
#' @export
plot_timeseries <- function(dquant, dfiltquant, omn=0, obs=NULL, modts=NULL, modi=1, baselining=NULL, seas='', xlim=NULL, ylim=NULL, add=FALSE, relative=FALSE, startyear=1901, endyear=2099, las=2, ylab='', scenario='rcp85', xaxt='s', yaxt='s', add.legend=TRUE, as.lines=FALSE, add.grid=FALSE){
  
  ## set up the plots
  dtime <- unique(unlist(lapply(dfiltquant, attr, 'time')))
  if (!add) {
    if (is.null(xlim)) xlim <- c(startyear, endyear)
    if (is.null(ylim)) {
      ylim <- range(dquant, dfiltquant, na.rm=T)
      if (!is.null(obs)) ylim <- range(ylim, obs, na.rm=T)
      ylim <- ylim + c(0, 0.1)*diff(ylim)
    }
    plot(0, type='n', xlim=xlim, ylim=ylim, xaxt='n', xlab='', ylab='', yaxt='n')
    if (relative){
      xlabels <- pretty(ylim*omn/100+omn)
      if (yaxt == 's') axis(2, at=(xlabels - omn)/omn*100, xlabels, las=las)
    } else {
      xlabels <- pretty(ylim + omn)
      if (yaxt == 's') axis(2, at=xlabels - omn, xlabels, las=las)
    }
    if (yaxt == 's') axis(2, at=mean(ylim), ylab, line=1.8*par('cex.axis'), tick=F)
    if (xaxt == 's') axis(1, at=seq(1900,2100, 50))
    if (!is.null(baselining)) rect(baselining[1], ylim[1] - diff(ylim)*.5, baselining[2], ylim[2] + diff(ylim)*.5, border=NA, col=grey(0.9))
  }
  leg.txt <- NULL
  leg.col <- NULL
  leg.lty <- NULL
  
  ## add in the polygon and ensemble median lines
  scen <- if (scenario %in% names(dquant)) scenario else names(dquant)[length(names(dquant))]
  ## plot the scenario and historical part in different colours
  for (sc in c(scen, 'historical')){
    enrange <- select_time(dfiltquant[[scen]], if (sc == scen) 2006 else startyear+9, if (sc == scen) endyear - 10 else 2005)
    equant <- select_time(dquant[[scen]], if (sc == scen) 2006 else startyear+9, if (sc == scen) endyear - 10 else 2005)
    if (!as.lines) vert.poly(equant[1,], equant[2,], x=attr(equant, 'time'), border=NA, col=scencols3[sc])
    ## range for 20-year averages
    vert.poly(enrange[1,], enrange[2,], x=attr(enrange, 'time'), border=NA, col=scencols[sc])
    ## Multi-model median time series
    plot(enrange, type='ts', lwd=3, si=3, col=scencols2[sc], add=T)
    ## range of yearly values
    if (as.lines) matplot(x=attr(equant, 'time'), t(equant[1:2,]), lwd=1, col=scencols[sc], type='l', add=T, lty=1)
  }
  
  if (!add & add.grid) grid(nx=NA, ny=NULL)
  
  ## add in observations
  if (!is.null(obs)){
    ocols <- if (oldcolours) hcl(300, seq(70, 40, length=length(obs)), seq(40, 70, length=length(obs))) else rep(obscol, length(obs))
    for (oi in rev(seq(along=obs))){
      plot(obs[[oi]], type='ts', si=1, add=T, col=ocols[oi], lwd=1)
    }
    leg.col <- c(leg.col, ocols)
    leg.txt <- c(leg.txt, paste('Observations (', gsub('0.25', '', names(obs)), ')', sep=''))
    leg.lty <- c(leg.lty, rep(1, length(ocols)))
  }
  
  ## add in individual model simulation
  if (!is.null(modts)){
    mcol <- if (oldcolours) hcl(60, l=50, c=50) else modcol
    if (length(modi) > 1) mcol <- hcl(seq(along=modi)/length(modi)*360, l=40, c=80)
    for (mi in seq(along=modi)){
      md <- select_time(select_models(select_ensembles(modts[[scen]], 1), modi[mi]), if (!is.null(obs)) floor(max(unlist(sapply(obs, attr, 'time')), na.rm=T)) else startyear + 9, endyear - 10)
      plot(md, type='ts', si=1, lwd=1, col=mcol[mi], add=T, lty=2 - mi %% 2)
      leg.txt <- c(leg.txt, paste('CMIP5 model (', rownames(modts[[scen]])[modi[mi]], ')', sep=''))
      leg.col <- c(leg.col, mcol[mi])
      leg.lty <- c(leg.lty, 2 - mi %% 2)
    }
    if (length(modi) > 1) leg.txt <- gsub('CMIP5 model \\(', '', gsub('\\)', '', leg.txt))
  }
  
  ## add in titles and legends
  if (!add){
    box()
    abline(h=0, lwd=1, lty=2)
    text(xlim[1], ylim[2], seas, cex=par('cex.axis'), adj=c(0,1), font=2)
    if (!is.null(leg.txt) & add.legend) legend('topleft', leg.txt, col=leg.col, lwd=2, inset=c(0.02,0.1), bty='n', cex=par('cex.axis')*0.9, lty=leg.lty)
  }
  
  return(ylim)
}
