#' Plot grouped projection bars with inner shading
#'
#' Bar plots grouped by scneario or seas with inner shading reflecting
#' natural variability.
#' 
#' @param meanchange list of percentile changes
#' @param meansd list of standard dev. or percentile changes for nat. var.
#' @param ylim vertical extent of plot
#' @param col,col2 colours for positive and negative segments of bars
#' @param lwd bar width
#' @param siglevel significance level to convert sdev. to shading ranges
#' @param lty line type for horizontal grid lines
#' @param distance relative distance between group of bars
#' 
#' @keywords plot
#' @export

alt_barproj <- function(meanchange, meansd=NULL, ylim=NULL, col=hcl(c(240,0), l=60, c=50), col2=hcl(c(240,0), l=30, c=80), lwd=10, siglevel=0.2, lty=1, distance=0.5){
  dpos <- seq(along=meanchange)
  names(dpos) <- names(meanchange)
  ## adjust colours to be able to have specific colours for all cases
  if (length(col) < 2*length(meanchange) | ! is.matrix(col)){
    col <- array(col, c(length(col), length(meanchange)))
    col2 <- array(col2, c(length(col2), length(meanchange)))
    colnames(col) <- colnames(col2) <- names(meanchange)
  } 
  cpos <- 1:ncol(meanchange[[1]])
  cpos <- (cpos - mean(cpos)) / max(cpos)*distance
  if (is.null(ylim)) ylim <- range(meanchange, 0, na.rm=T)
  plot(0, type='n', xlim=range(dpos) + c(-0.5,0.5), ylim=ylim, xaxt='n', xlab='', ylab='', yaxt='n')
  grid(nx=NA, ny=NULL, lwd=1, lty=lty)
  abline(h=0, lwd=1, lty=2)
  dfact <- qnorm(1 - 0.5 * siglevel)      
  for (mn in names(meanchange)){
    for (cc in seq(along=cpos)){
      plotcolbar(x=dpos[mn] + cpos[cc], y=meanchange[[mn]][,cc], yshade=if (!is.null(meansd) & length(meansd[[mn]]) == 1) meansd[[mn]][1]*dfact else if (!is.null(meansd) & length(meansd[[mn]]) == 3) meansd[[mn]] else NULL, lwd=lwd, col=col[,mn], col2=col2[,mn])
    }
  }
  box()
}
