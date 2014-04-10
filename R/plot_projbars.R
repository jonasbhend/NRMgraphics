#' @title Barplots of projected changes
#'
#' @description
#' Function to plot bars of projected changes
#'
#' @param meanchange list of projected n-year mean change (seasons // scenario)
#' @param allchange list of projected annual changes (seasons // scenario)
#' @param ylim vertical extent of plot
#' @param nmods,nsim number of models and simulations per scenario
#' @param add.legend logical, should legend be added?
#' @param yside side at which y-axis is labelled (2,4)
#' @param add.rect logical, should negative values have grey background shading?
#' @param ylab vertical axis labelling
#' @param las,xlas orientation o y and x axis labelling
#' @param title title for the plot
#' @param scenarios vector of scenario names to be plotted
#' @param add.nmod logical, should number of models and simulations be displayed?
#' @param seas which seasons should be plotted
#' @param lwd,lwd2 line width of bars and bar extensions
#' @param cex character expansion for text
#' @param legend.pos placement of legend (see \code{\link{legend}})
#' @param add logical, should plot be added to existing plot?
#' @param scols,scols2 scenario colours (named vector)
#' @param yaxt character describing y-axis labelling
#' @param distance white space between group of bars
#' 
#' @return vertical limits of plots (e.g. \code{ylim})
#' 
#' @keywords plot
#' @export
plot_projbars <- function(meanchange, allchange=NULL, ylim=NULL, nmods=NULL, nsim=NULL, add.legend=TRUE, yside=4, add.rect=TRUE, ylab='', las=2, xlas=2, title='', scenarios=NULL, add.nmod=FALSE, seas=NULL, lwd=10, lwd2=2, cex=par('cex.axis'), legend.pos='topright', add=FALSE, scols=scencols, scols2=scencols2, yaxt='s', distance=0.5){
  
  inseas <- seas
  if (is.list(meanchange[[1]])){
    if (is.null(seas)){
      if (all(c('DJF', 'MAM', 'JJA', 'SON') %in% names(meanchange))){
        seas <- c('DJF', 'MAM', 'JJA', 'SON')
      } else {
        seas <- names(meanchange)
      }
    }
    if (all(seas == '')) seas <- 1
    meanchange <- meanchange[seas]
    if (!is.null(allchange)) allchange <- allchange[seas]
    cpos <- seq(along=meanchange)
    scens <- names(meanchange[[1]])
  } else {
    cpos <- seq(1, ncol(meanchange[[1]]))
    scens <- names(meanchange)
  }
  scenarios <- if (is.null(scenarios)) scens else intersect(scenarios, scens)
  dpos <- seq(along=scenarios)
  dpos <- (dpos - mean(dpos))/max(dpos)*if (length(scenarios) < 4) distance else distance/5*7
  names(dpos) <- scenarios
  xlim <- range(cpos) + c(-0.5, 0.5)
  if (is.null(ylim)) {
    ylim <- range(meanchange, allchange, 0, na.rm=T)
    ylim <- ylim + c(0, 0.1)*diff(ylim)
  }
  ## cross-check the seasons
  if (!add){
    plot(0, type='n', xlim=xlim, ylim=ylim, xaxt='n', yaxt='n', xlab='', ylab='')
    if (add.rect) rect(xlim[1] - diff(xlim) * 0.2, min(-2, ylim[1] - diff(ylim)*0.2), xlim[2] + diff(xlim)*0.2, 0, border=NA, col=grey(0.85))
    grid(nx=NA, ny=NULL)
    abline(h=0, lwd=1, lty=2)
    if (yaxt == 's') axis(yside, las=las)
    if (yaxt == 's') axis(yside, mean(ylim), ylab, line=2, tick=F)
  }
  
  ## get scenario colours
  if (is.null(scols) | ! all(scenarios %in% names(scols))){
    scols <- rep(hcl(0, l=c(50,70), c=c(70,50)), length=length(scenarios))
    scols2 <- rep(hcl(0, l=c(30,50), c=c(50,30)), length=length(scenarios))
    names(scols) <- names(scols2) <- scenarios
  }
  
  
  ## loop through seasons and scenarios to plot bars
  for (se in seq(along=cpos)){
    for (scen in scenarios){
      ## plot bars in right part of panel indicating range
      if (is.list(meanchange[[1]])){
        if (!is.null(meanchange[[se]][[scen]])) projbar(x=dpos[scen] + cpos[se], y=meanchange[[se]][[scen]], y2=allchange[[se]][[scen]], col=scols[scen], col2=scols2[scen], lwd=lwd, lwd2=lwd2)
      } else {
        projbar(x=dpos[scen] + cpos[se], y=select_seas(meanchange[[scen]], (se - 1)/length(cpos)), y2=select_seas(allchange[[scen]], (se - 1)/length(cpos)), col=scols[scen], col2=scols2[scen], lwd=lwd, lwd2=lwd2)
      }
      if (length(cpos) == 1 & !is.null(nmods)) {
        if (add.nmod) text(cpos[se] + dpos[scen], ylim[2] - 0.13*diff(ylim),paste(scennames[scen],'\n', nmods[scen],' (', nsim[scen],')', sep=''), adj=c(0.5, 0), cex=cex*0.8)  
        if (se == 1 & scen == scens[1]) text(mean(xlim), ylim[2], '2080-2099', adj=c(0.5, 1), cex=cex)
      }
    }
  }
  text(xlim[1], ylim[2], title, font=2, adj=c(0,1), cex=1.2*cex)
  
  ## add in legend
  if (add.legend & length(cpos) != 1){
    if (add.nmod){
      legend(legend.pos, paste(scennames[scenarios], ', ', nmods[scens], ' (', nsim[scens],')', sep=''), col=scols[scenarios], lwd=2, bty='n', inset=if (title != '' & legend.pos == 'topleft') c(0.02, 0.1) else 0.02, title='Scenario, #mod (#sim)', cex=cex)
    } else {
      legend(legend.pos, scennames[scenarios], col=scols[scenarios], lwd=2, bty='n', inset=if (title != '' & legend.pos == 'topleft') c(0.02, 0.1) else 0.02, cex=cex)
    }
  }  
  if (length(cpos) == length(seas) & !is.null(inseas)) axis(1, at=cpos, if (all(toupper(seas) %in% names(seastxt))) seastxt[toupper(seas)] else seas, tick=F, line=-0.5, las=xlas)
  if (!add) box()
  
  ## return the position of the individual bars
  invisible(outer(cpos, dpos, '+'))
}

