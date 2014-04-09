#' @title Projection bars grouped by method
#'
#' @description
#' Plots grouped bars (by method) with alternating colours
#'
#' @param meanchange list of projected n-year mean change (method; scenario)
#' @param allchange list of projected annual changes (method; scenario)
#' @param x named vector with x intersects
#' @param ylim vertical extent of plot
#' @param add.legend logical, should legend be added?
#' @param yside side at which y-axis is labelled (2,4)
#' @param add.rect logical, should negative values have grey background shading?
#' @param ylab vertical axis labelling
#' @param las,xlas orientation o y and x axis labelling
#' @param title title for the plot
#' @param lwd,lwd2 line width of bars and bar extensions
#' @param cex character expansion for text
#' @param legend.pos placement of legend (see \code{\link{legend}})
#' @param add logical, should plot be added to existing plot?
#' @param colval hue(s) for colours in barplot
#' @param coli index for dark or light shading of bars (alternating is default)
#' @param axes logical, should axes be plotted?
#' 
#' @keywords plot
#' @export
plot_methbars <- function(meanchange, allchange=NULL, x=NULL, ylim=NULL, add.legend=FALSE, yside=2, add.rect=TRUE, ylab='', las=2, xlas=2, title='', lwd=10, lwd2=2, cex=par('cex.axis'), legend.pos='topright', add=FALSE, colval=0, coli=NULL, axes=TRUE){
  
  if (is.null(x)) {
    x <- seq(along=meanchange)
    ctypes <- names(meanchange)
  } else {
    ctypes <- names(x)
  }
  xlim <- range(x)
  if (length(x) < 5) {
    xlim <- xlim + c(-0.3, 0.3)*diff(xlim)
  } else if (length(x) <= 10) {
    xlim <- xlim + c(-0.1, 0.1)*diff(xlim)
  } else if (length(x) > 10 & length(x) <= 20) {
    xlim <- xlim + c(-0.03, 0.03)*diff(xlim)
  }
  if (is.null(ylim)) {
    ylim <- range(meanchange[ctypes], allchange, 0, na.rm=T)
    ylim <- ylim + c(0, 0.1)*diff(ylim)
  }
  if (!add){
    plot(0, type='n', xlim=xlim, ylim=ylim, xaxt='n', yaxt='n', xlab='', ylab='')
    if (add.rect) rect(xlim[1] - diff(xlim) * 0.2, min(-2, ylim[1] - diff(ylim)*0.2), xlim[2] + diff(xlim)*0.2, 0, border=NA, col=grey(0.85))
    grid(nx=NA, ny=NULL)
    abline(h=0, lwd=1, lty=2)
    if (axes) axis(yside, las=las)
    if (axes) axis(yside, mean(ylim), ylab, line=2, tick=F)
  }
  
  ## get scenario colours
  ccl <- if (colval > 30 & colval < 70) 65 else 50
  if (is.null(coli)) coli <- rep(1:2, length=length(x)) else if (length(coli) != length(x)) coli <- rep(1:2, length=length(x))
  ccols <- hcl(colval, l=ccl + c(15, 25), c=ccl - c(5, 15))[coli]
  ccols2 <- hcl(colval, l=ccl - c(10, 0), c=ccl + c(10,0))[coli]
  
  ## plot bars in right part of panel indicating range
  projbar(x=x, y=sapply(meanchange[ctypes], function(x) x), y2=if (!is.null(allchange)) sapply(allchange[ctypes], function(x) x) else NULL, col=ccols, col2=ccols2, lwd=lwd, lwd2=lwd2)
  ## add title
  text(xlim[1], ylim[2], title, font=2, adj=c(0,1), cex=1.2*cex)
  
  if (!add) box()
}



