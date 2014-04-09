#' @title Function to add a single coloured bars
#'
#' @description
#' Plot bar of projected changes at specified location with light
#' shading of inner region (generally used to indicate natural variability).
#'
#' @param x location on the horizontal axis to plot
#' @param y vector of percentiles (e.g. 10, 90, 50th)
#' @param yshade value for inner shading (see details)
#' @param col colour for negative and positive values
#' @param col2 colour for horizontal line (median)
#' @param lwd line width of bar
#' 
#' @details
#' Inner shading can be indicated both using a single value (per bar)
#' or two values per bar for bottom and top extend if asymmetrical.
#' 
#' @keywords plot
#' @export
plotcolbar <- function(x, y, yshade=NULL, col=hcl(c(240,0), l=60, c=50), col2=hcl(c(240,0), l=30, c=80), lwd=10){
  if (y[2] > 0) segments(x0=x, y0=max(y[1], 0), y1=y[2], col=col[2], lwd=lwd, lend=3)
  if (y[1] < 0) segments(x0=x, y0=y[1], y1=min(y[2], 0), col=col[1], lwd=lwd, lend=3)
  ## check whether natural variability should be shaded
  yshade <-  if (length(yshade) == 1) c(-1,1)*yshade else yshade[1:2]
  if (!is.null(yshade)){
    if ((min(y) < max(yshade) & max(y) > min(yshade)) | (max(y) > min(yshade) & min(y) < max(yshade))) segments(x0=x, y0=max(min(y), min(yshade)), y1=min(max(y), max(yshade)), lwd=lwd, lend=3, col=rgb(1,1,1,0.5))
  }
  segments(x0=x, y0=y[3] - diff(par('usr')[3:4])*0.005, y1=y[3] + diff(par('usr')[3:4])*0.005, col=col2[(sign(y[3]) + 1)/2 + 1], lwd=lwd*1.2, lend=3)
}
