#' Scatterplot of downscaled vs. GCM data
#'
#' Plots a scattter plot of downscaled vs. GCM data with corresponding 
#' regression line (or correlation) for different scenarios.
#'
#' @param df list of data.frames with model data (see details)
#' @param xlim,ylim horizontal and vertical extent of plot
#' @param add.names logical, should model names be added?
#' @param correlation logical, show correlation instead of regression?
#' @param ... additional arguments passed to \code{\link{plot}}
#' 
#' @details
#' The data frames (one for each scenario) are expected to contain 
#' elements named Downscaled and GCM. In addition, for \code{add.names} to 
#' work, the rows need to contain the model names.
#' 
#' @keywords plot
#' @export
downscaling_scatterplot <- function(df, xlim=range(df, na.rm=T), ylim=range(df, na.rm=T), add.names=FALSE, correlation=FALSE, ...){
  scens <- names(df)
  models <- sort(unique(unlist(lapply(df, rownames))))
  modchar <- letters[seq(along=models)]
  names(modchar) <- models
  plot(Downscaled ~ GCM, data=df[[scens[1]]], type='n', xlim=xlim, ylim=ylim, ...)
  grid()
  abline(c(0,1), lwd=2)
  leg.txt <- NULL
  for (scen in scens){
    points(Downscaled ~ GCM, data=df[[scen]], pch=modchar[rownames(df[[scen]])], cex=par('cex.axis')*1.2, col=scencols2[scen])
    if (correlation){
      xycor <- round(cor(df[[scen]]$GCM, df[[scen]]$Downscaled, use='p'), 2)
      leg.txt <- c(leg.txt, paste(toupper(scen), ': corr. ', xycor, sep=''))
      xylm <- lm(Downscaled ~ GCM, data=df[[scen]])
      abline(coef(xylm), lwd=2, col=scencols2[scen], lty=2)
    } else {
      xylm <- lm(Downscaled ~ GCM - 1, data=df[[scen]])
      xycoef <- format(coef(xylm), digit=2)
      abline(c(0,coef(xylm)), lwd=2, col=scencols2[scen], lty=2)
      leg.txt <- c(leg.txt, paste(toupper(scen), ': y = ', xycoef, 'x', sep=''))
    }
    if (add.names){
      xi <- which.max(abs(df[[scen]]$GCM - df[[scen]]$Downscaled))
      xpos <- unlist(df[[scen]][xi,])
      tpos <- xpos + sign(diff(xpos)) * c(-1,1)*diff(xlim)*0.1
      tpos2 <- xpos + sign(diff(xpos)) * c(-1,1)*diff(xlim)*0.08
      text(tpos[1], tpos[2], rownames(df[[scen]])[xi], adj=if(sign(diff(xpos)) + 1) c(1,1) else c(0, 0), cex=par('cex.axis'))
      arrows(x0=xpos[1], y0=xpos[2], x1=tpos2[1], y1=tpos2[2], code=1, lwd=2, length=0.05)
    }
  }
  legend('topleft', leg.txt, fill= if (correlation) scencols2[scens] else NULL, col= if (correlation) NA else  scencols2[scens], lwd=if (correlation) NA else 2, cex=par('cex.axis'), lty=2, bty='n', inset=c(0,0.1))
  invisible(modchar)  
}
