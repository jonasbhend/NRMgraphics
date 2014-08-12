#' @name allAustralia
#' @aliases allAustraliaKey
#' 
#' @title
#' Australian overview plots
#' 
#' @description
#' This function provides the backbone for the Australian overview plots.
#' In essence, it provides the map and the locations where to add insets.
#' Also \code{allAustraliaKey} offers the additional functionality of 
#' placing a legend in the bottomleft corner.
#' 
#' @param fun function to be executed at the respective spots for insets
#' @param bg list containing information to colour the map background (see details)
#' @param add.legend logical, should legend for map background be added?
#' @param old logical, should old regions be used (RE,RW instead of RN,RS)?
#' @param regnames which regions should be plotted (see details)
#' @param ylim vertical extent of background map for control of additional whitespace
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
#' @examples
#' bbg <- list(data=c(R=0.9, SA=1.9, `NA`=2.9, EA=3.9))
#' afun <- function(regn) {
#'      plot(rnorm(100), type='l', xlab='', ylab='', axes=F)
#'      box()
#'      text(1, par('usr')[4], regn, font=2, adj=c(0,1.5))
#'      }
#' allAustralia(fun=afun, bg=bbg, regnames=c('R', 'NA', 'SA', 'EA'))
#' @export
allAustralia <- function(fun, bg=NULL, add.legend=TRUE, old=FALSE, regnames=NULL, maplim=c(-51, -8), ...){
  ## read in the nrm boundaries
  data('nrmregions', envir=environment())
  ## set the map dimensions
  xlim <- c(110, 161)
  nrmlons <- c(WT=148, ECS=156, RE=135, MB=143.5, RW=123, MNW=129, SSVW=138, CS=149, SSVE=152, MNE=142, SSWFE=132.5, SSTE=150, SSWFW=116, SSTW=143, ECN=154.5, RN=134, RS=(132.5 + 116) / 2, `NA`=135, SA=135, EA=149, R=121)
  nrmlats <- c(WT=-14, ECS=-31, RE=-25, MB=-34, RW=-25, MNW=-14, SSVW=-38, CS=-29, SSVE=-38, MNE=-19, SSWFE=-35, SSTE=-45, SSWFW=-33, SSTW=-45, ECN=-23, RN=-23, RS=-29, `NA`=-14, SA=-38, EA=-26, R=-26)
  plot(0, type='n', xlim=xlim, ylim=maplim, asp=1/cos(mean(maplim)/180*pi), axes=F, xlab='', ylab='', xaxs='i', yaxs='i')
  ## plot background
  regnames2 <- if (all(regnames %in% c('EA', 'SA', 'NA', 'R')) & !is.null(regnames)) regnames else setdiff(setdiff(names(nrmlons), c('NA', 'SA', 'EA', 'R')), if (old) c('RN', 'RS') else c('RE', 'RW'))
  if (is.null(regnames)) regnames <- regnames2
  if (!is.null(bg)){
    if (!is.list(bg)){
      bg <- list(data=bg)
    }
    if (is.null(bg$lev)) bg$lev <- pretty(bg$data)
    if (is.null(bg$col)) bg$col <- colourramp(length(bg$lev) - 1, ramp='redblue', start=0.2)
    for (regn in regnames2) lapply(nrmregions[[regn]], polygon, border=grey(0.7), col=bg$col[cut(bg$data[regn], bg$lev)])
    if (add.legend){
      addcolourbar <- function(){
        image(seq(bg$col), 1, as.matrix(seq(along=bg$col)), breaks=seq(0.5, length(bg$col)+0.5), col=bg$col, xaxt='n', yaxt='n', xlab='', ylab='')
        box()
        abline(v=seq(1.5, length(bg$col) - 0.5))
        axis(1, at=seq(1.5, length(bg$col) - 0.5), labels=bg$lev[-c(1, length(bg$lev))], cex.axis=0.8*par('cex.axis'), line=-0.7, tick=F)
        if (!is.null(bg$units)) axis(1, at=length(bg$col)+0.5, bg$units, tick=F, hadj=0.2, cex.axis=0.8*par('cex.axis'), line=-0.7)
        if (!is.null(bg$title)) axis(3, at=0.5, bg$title, font=2, tick=F, hadj=0, line=-0.7, cex.axis=0.8*par('cex.axis'))        
      }
      ## add the colourbar in an inset
      inset(inset.x=123.8, inset.y=maplim[1]+2, fun=addcolourbar, inset.size=c(0.4, 0.015))
    }
  } else {
    for (regn in regnames2) lapply(nrmregions[[regn]], polygon, border=grey(0.7), col=grey(0.9))    
  }
  map('worldHires', regions='Australia', add=T, lwd=2, col=grey(0.5))
  ## loop over regions and apply main function
  if (all(!is.na(regnames))){
    size <- if (all(regnames %in% c('EA', 'SA', 'NA', 'R'))) c(0.35, 0.2) else c(0.1, 0.15)
    for (regn in regnames){
      if (regn == 'SSVW') arrows(x0=140.5, x1=143.5, y0=-39, y1=-38, code=2, lwd=3, length=0.05, col=grey(0.2))
      inset(inset.x=nrmlons[regn], inset.y=nrmlats[regn], inset.size=size, inset.bg='#FFFFFFCC', fun=fun, regn=regn, ...=...)
    }
  }
  ## plot bounding box
  box()
}

#' @rdname allAustralia
#' @export
allAustraliaKey <- function(fun, ...){
  inset(inset.x=116, inset.y=-43, inset.size=c(0.1, 0.15), fun=fun, ...=...)
}
