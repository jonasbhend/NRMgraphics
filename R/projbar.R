#' Add multiple projections bars (with extensions)
#'
#' Simple function to add multiple projections bars with extensions
#' to existing plot.
#' 
#' @param x horizontal intersects for bars
#' @param y matrix with array of bars (percentiles; bars)
#' @param y2 matrix with extensions to bars
#' @param col,col2 colours for bar shading and vertical line (median)
#' @param lwd,lwd2 bar width and extensions width
#' 
#' @keywords plot
#' @export
projbar <- function(x, y, y2=NULL, col='grey', col2='black', lwd=10, lwd2=2){
  if (!is.null(y2)){
    if (!is.matrix(y2)) y2 <- as.matrix(y2)
    ## arrows(x0=x, y0=y2[1,], y1=y2[2,], lwd=lwd2, angle=90, length=0.03*lwd2, lend=3, code=3, col=col)
    segments(x0=x, y0=y2[1,], y1=y2[2,], lwd=lwd2, lend=3, col=col)
  }
  if (!is.matrix(y)) y <- as.matrix(y)
  segments(x0=x, y0=y[1,], y1=y[2,], lwd=lwd, lend=3, col=col)
  ydiff <- diff(par('usr')[3:4])*0.005
  segments(x0=x, y0=y[3,]-ydiff, y1=y[3,]+ydiff, lwd=lwd*1.2, lend=3, col=col2)
}

