#' Plot time series (plume)
#'
#' Function to plot timeseries given objects of class 'NetCDF'
#'
#' @param dquant list of (outer) quantile ranges for different scenarios
#' @param dfiltquant list of (inner) quantile ranges
#' @param omn Climatology to be added to everything (multiplied if \code{relative})
#' @param obs list or array of observations to be added as lines (one each)
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
#' @examples
#' ## compute anomalies of the original time series
#' danom <- anomalies(dd, c(1950,2005))
#' oanom <- anomalies(oo, c(1950, 2005))
#' ## compute the quantiles of 20-year means (inner shading)
#' dfiltquant <- apply.NetCDF(applyfilter(danom, rep(1/20, 20)), 3, quantfun)
#' ## compute the quantiles of individual years (outer shading)
#' dquant <- applyfilter(apply.NetCDF(danom, 3, quantfun), rep(1/20, 20))
#' 
#' ## plot the time series 
#' plot_timeseries(dquant=dquant,
#'                 dfiltquant=dfiltquant,
#'                 obs=oanom,
#'                 ylab='Temperature anomalies (1950-2005)')
#' 
#' @keywords plot
#' @export
plot_timeseries <- function(dquant, dfiltquant, omn=0, obs=NULL, modts=NULL, modi=1, baselining=NULL, seas='', xlim=NULL, ylim=NULL, add=FALSE, relative=FALSE, startyear=NULL, endyear=NULL, las=2, ylab='', scenario='rcp85', xaxt='s', yaxt='s', add.legend=TRUE, as.lines=FALSE, add.grid=FALSE){
  
  ## by default the function assumes that data is from rcp85
  if (!is.list(dquant)){
    dquant <- list(dquant)
    dfiltquant <- list(dfiltquant)
    names(dquant) <- names(dfiltquant) <- scenario
  }
  
  ## check that objects are of type NetCDF
  if (class(dquant[[1]]) != 'NetCDF'){
    warning('Object not of class NetCDF\nautomatically transformed assuming time series starts in 1901')
    convert2NetCDF <- function(x){
      xout <- x
      attr(xout, 'time') <- 1900 + 1:dim(x)[length(dim(x))]
      class(xout) <- 'NetCDF'
      return(xout)  
    }
    dquant <- lapply(dquant, convert2NetCDF)
    dfiltquant <- lapply(dfiltquant, convert2NetCDF)
  }
  
  ## figure out start and endyear if not present
  if (is.null(startyear)) startyear <- min(attr(dquant[[1]], 'time'))
  if (is.null(endyear)) endyear <- max(attr(dquant[[1]], 'time'))
  
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
  scens <- if (all(scenario %in% names(dquant))) scenario else names(dquant)[length(names(dquant))]
  ## get the scenario and historical part
  equant <- enrange <- list()
  for (sc in c(rev(scens), 'historical')){
    scen <- if (sc == 'historical') scens[1] else sc
    enrange[[sc]] <- select_time(dfiltquant[[scen]], if (sc == scen) 2006 else startyear+9, if (sc == scen) endyear - 10 else 2005)
    equant[[sc]] <- select_time(dquant[[scen]], if (sc == scen) 2006 else startyear+9, if (sc == scen) endyear - 10 else 2005)
    if (!as.lines) {
      vert.poly(equant[[sc]][1,], equant[[sc]][2,], x=attr(equant[[sc]], 'time'), border=NA, col=scencols3[sc])
    }
    ## range for 20-year averages
    vert.poly(enrange[[sc]][1,], enrange[[sc]][2,], x=attr(enrange[[sc]], 'time'), border=NA, col=scencols[sc])
    ## Multi-model median time series
    plot(enrange[[sc]], type='ts', lwd=3, si=3, col=scencols2[sc], add=T)
    ## range of yearly values
    if (as.lines) matplot(x=attr(equant[[sc]], 'time'), t(equant[[sc]][1:2,]), lwd=1, col=scencols[sc], type='l', add=T, lty=1)
  } ## end of loop on scenarios
  
  if (!add & add.grid) grid(nx=NA, ny=NULL)
  
  ## add in observations
  if (!is.null(obs)){
    nobs <- if (is.list(obs)) length(obs) else nrow(obs)
    ocols <- if (oldcolours) hcl(300, seq(70, 40, length=nobs), seq(40, 70, length=length(obs))) else rep(obscol, nobs)
    for (oi in nobs:1){
      plot(if (is.list(obs)) obs[[oi]] else obs, 
           type='ts', 
           si=if (is.list(obs)) 1 else oi, 
           add=T, 
           col=ocols[oi], 
           lwd=1)
    }
    leg.col <- c(leg.col, ocols)
    onames <- if (is.list(obs)) names(obs) else rownames(obs)
    leg.txt <- c(leg.txt, paste('Observations (', gsub('0.25', '', onames), ')', sep=''))
    leg.lty <- c(leg.lty, rep(1, length(ocols)))
  }
  
  ## add in individual model simulation
  if (!is.null(modts)){
    scen <- scens[1]
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
  
  invisible(ylim)
}
