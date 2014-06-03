#' Add water mark
#' 
#' Function to plot a watermark stamp to the bottom right of the plot
#' 
#' @param txt character string to be displayed
#' @param cex character expansion factor
#' 
#' @examples
#' plot(rnorm(100))
#' addWatermark()
#' 
#' @keywords aplot
#' @export 
addWatermark <- function(txt=NULL, cex=par('cex.axis')*0.5){
  if (is.null(txt)){
    txt <- paste(firstcap(Sys.getenv('USER')), format(Sys.time(), '%d/%m/%Y'), sep=', ')
  }
  text(par('usr')[2] - 0.01*diff(par('usr')[1:2]), 
       par('usr')[3], 
       txt, 
       adj=c(1, -0.5), 
       cex=cex)
}