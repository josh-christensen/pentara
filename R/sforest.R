#' @title Classic forest plot
#'
#' @description Creates forest plot that matches Pentara's historic forest plot
#'   style.
#'
#' @param dat data for the plot
#' @param res a string naming a column to plot as points
#' @param ci.lb a string naming column to plot as ci upper bounds
#' @param ci.ub a string naming column to plot as ci lower bounds
#' @param ordervar a string naming a column that defines the order of sections.
#'   The column must be numeric and have the same value for all rows in the same
#'   section.
#' @param orderlab a string naming a column of section labels. The column must
#'   be character and have the same value for all rows in the same section.
#' @param suborder a string naming a column that defines the order within
#'   sections. The column must be numeric and have different values for all rows
#'   in the same section.
#' @param sublab a string naming a column of row labels. The column must be
#'   character and have different values for all rows in the same section.
#' @param xlab a string to be used as the x-axis label
#' @param footnote a string to use as a footnote
#' @param varlist a character vector containing the names of other columns to be
#'   included in the plot
#' @param varlabs a character vector containing the labels to be used in the
#'   plot for variables in varlist
#' @param pvals a character vector containing the names of columns in varlist
#'   that should be formatted as p-values
#' @param refline a numeric value indicating where the dashed vertical reference
#'   line should be drawn
#' @param col color used for the background bands
#' @param wd1 numeric adjustment for the space between the left text and the
#'   plot
#' @param wd2 numeric adjustment for the space between the right text and the
#'   plot
#' @param alim adjust the x-axis limits for the plot
#' @param col1 a string used to label the column of text on the left of the plot
#' @param bold.col color for bold p-values
#' @param latex.out draws only a box around what won't get cropped by latex
#' @param express logical. If TRUE, do not coerce sublab variable to character
#'   and do not automatically indent
#' @param expresso logical. if TRUE, do not coerce orderlab variable to
#'   character
#' @param highbold logical. Currently must be FALSE
#' @param digits numeric vector indicating how many digitis should be displayed
#'   after the decimal palce in the estimate column and on the x-axis. If only
#'   one number is provided it is used for both
#' @param refcol color for the reference line
#' @param show logical vector of the same length as the number of rows in the
#'   plot indicating which p-values should be displayed
#' @param nobold logical. If TRUE do not bold significant p-values
#' @param wgts numeric vector that shifts added text columns multiplicatively
#'   relative to the plot
#' @param trendthresh threshold for p-values identifying a trend
#' @param trend.col color to use in identifying p-values that show a trend
#' @param at.lab vector of labels for x-axis ticks. Must be used with at and
#'   have the same length as at
#' @param at vector of numeric values at which to place x-axis ticks
#' @param ci.lab string used to label the default column in the form '`string`
#'   \[95% CI\]'
#' @param na.action na.action to be used within sforest
#' @param pdig number of digits to display for p-values
#' @param graph.lab label for the top of the plot
#' @param ci.correct number used to move the default CI column to the left
#' @param scale.override numeric vector of the same length as at.lab that
#'   creates a new x-axis spanning the entire graphic. Rarely useful
#' @param ... Further arguments to be passed to various subplotting functions
#'
#' @return invisibly returns NULL
#'
#' @examples
#' forest_dat <- data.frame(
#'   mean = c(.1, .5, .2, .8),
#'   lower = c(.1, .5, .2, .8) - qnorm(.975),
#'   upper = c(.1, .5, .2, .8) + qnorm(.975),
#'   ordervar = rep(c(1,2), each = 2),
#'   orderlab = rep(c("Study 1", "Study 2"), each = 2),
#'   subordervar = rep(c(1,2), times = 2),
#'   suborderlab = rep(c("Endpoint 1", "Endpoint 2"), times = 2),
#'   p_val = c(.3756, .1023, .1542, .043)
#' )
#'
#' sforest(
#'   dat = forest_dat,
#'   res = "mean",
#'   ci.lb = "lower",
#'   ci.ub = "upper",
#'   ordervar = "ordervar",
#'   orderlab = "orderlab",
#'   suborder = "subordervar",
#'   sublab = "suborderlab",
#'   varlist = c("p_val"),
#'   varlabs = c("P-Value"),
#'   pvals = c("p_val"),
#'   xlab = "Treatment Effect",
#'   latex.out = FALSE
#' )
#' @export

sforest <- function(dat,res,ci.lb,ci.ub,ordervar,orderlab,suborder,sublab,xlab,
                    footnote="",varlist=NULL,varlabs=NULL,pvals=NULL,refline=0,
                    col="aliceblue",wd1=0.5,wd2=0.5,alim=NULL,col1=" ",bold.col="black",
                    latex.out=TRUE,express=FALSE,expresso=FALSE,highbold=FALSE,digits=2,
                    refcol="black",show=NULL,nobold=FALSE,wgts=NULL,trendthresh=0.05,trend.col="black",
                    at=NULL,at.lab=NULL,ci.lab=NULL,na.action=NULL,pdig=4,graph.lab="Graphical Summary",ci.correct=0,
                    scale.override=NULL,...) {

  if(!is.null(na.action)) {
    old.na.action = options()$na.action
    options(na.action=na.action)
  }
  ### decrease margins so the full space is used
  if(footnote!="") graphics::par(mar=c(4.5,4,1,2),font=1) else graphics::par(mar=c(4,4,1,2),font=1)

  ordervar <- as.numeric(as.character(dat[,ordervar]))
  suborder <- as.numeric(as.character(dat[,suborder]))
  new.order <- order(ordervar,suborder,decreasing=T)
  dat <- dat[new.order,]

  ordervar <- rev(ordervar[new.order])
  suborder <- rev(suborder[new.order])

  res <- as.numeric(as.character(dat[,res]))
  ci.lb <- as.numeric(as.character(dat[,ci.lb]))
  ci.lb[is.na(ci.lb)] <- res[is.na(ci.lb)]
  ci.ub <- as.numeric(as.character(dat[,ci.ub]))
  ci.ub[is.na(ci.ub)] <- res[is.na(ci.ub)]
  vars <- cbind(dat[,varlist])
  if(!express) slab <- paste0("  ",as.character(dat[,sublab])) else slab <- dat[,sublab]
  if(!expresso) orderlab <- as.character(dat[,orderlab]) else orderlab <- dat[,orderlab]
  olab <- NULL
  for(i in seq(along=orderlab)) if(!orderlab[i] %in% olab) olab <- c(olab,orderlab[i])
  #  olab <- unique(orderlab)

  plot.lim <- range(pretty(x = c(min(ci.lb,na.rm=T),
                                 max(ci.ub,na.rm=T), n = 4)))

  plcheck <- min(plot.lim/c(min(ci.lb,na.rm=T), max(ci.ub,na.rm=T)),na.rm=T)
  if(plcheck > 2) {
    div <- floor(plcheck/(10**floor(log10(plcheck))))*(10**floor(log10(plcheck)))
    plot.lim <- plot.lim/div
  }
  atest <- (refline-plot.lim[1])/diff(plot.lim)-(0:4)/4
  matest <- which(abs(atest) == min(abs(atest)))[1]
  if(matest==5) {
    plot.lim <- c(plot.lim[1],refline)
  } else if(matest==1) {
    plot.lim <- c(refline,plot.lim[2])
  } else if(atest[matest]<0) {
    plot.lim <- c(plot.lim[2]-4*(plot.lim[2]-refline)/(5-matest),plot.lim[2])
  } else plot.lim <- c(plot.lim[1],plot.lim[1]+4*(refline - plot.lim[1])/(matest-1))

  if(is.null(alim)) {
    low.plot <- plot.lim[1]
    high.plot <- plot.lim[2]
  } else {
    atest <- (refline-alim[1])/diff(alim)-(0:4)/4
    matest <- which(abs(atest) == min(abs(atest)))[1]
    if(matest==5) {
      alim <- c(alim[1],refline)
    } else if(matest==1) {
      alim <- c(refline,alim[2])
    } else if(atest[matest]<0) {
      alim <- c(alim[2]-4*(alim[2]-refline)/(5-matest),alim[2])
    } else alim <- c(alim[1],alim[1]+4*(refline - alim[1])/(matest-1))
    low.plot <- alim[1]
    high.plot <- alim[2]
  }
  if(is.null(alim)) {
    midlim <- mean(c(low.plot,high.plot))
    if(midlim < min(ci.lb,na.rm=T)) low.plot <- midlim
    if(midlim > max(ci.ub,na.rm=T)) high.plot <- midlim
    alim <- c(low.plot,high.plot)
  }
  if(is.null(show)) show = rep(TRUE,nrow(dat))

  range.plot <- high.plot - low.plot
  nvars = ncol(vars)
  slabchar <- max(nchar(c(olab,slab)))/10
  last.wd <- 1
  last.sum <- max(0,floor(log10(abs(res))),na.rm=TRUE)+max(0,floor(log10(abs(ci.lb[!is.na(ci.lb)]))))+max(0,floor(log10(abs(ci.ub[!is.na(ci.ub)]))))-3
  if(last.sum>0) last.wd <- 1+0.1*last.sum
  high.all <- high.plot+(nvars+last.wd)*wd2*slabchar*range.plot
  low.all <- low.plot-wd1*slabchar*range.plot
  ha <- high.all+0.5*range.plot
  la <- low.all-0.5*range.plot
  nrows = length(res)+length(olab)+2
  row1 <- length(ordervar)+seq(along=unique(ordervar))-rev(c(unname(by(seq(along=ordervar),ordervar,utils::tail,n=1))))
  row2 <- length(ordervar)+seq(along=unique(ordervar))-rev(c(unname(by(seq(along=ordervar),ordervar,utils::head,n=1))))
  poly.x <- c(la,rep(c(ha,ha,la,la),times=ceiling(length(row1)/2)))
  if(length(row1) %% 2 ==0) poly.y <- c(rep(row2+1.5,each=2),row2[1]+1.5) else poly.y <- c(rep(row1-.5,each=2),rep(row2[length(row2)]+1.5,2),row1[1]-.5)
  rows <- eval(parse(text=paste0("c(",paste(paste(row1,row2,sep=":"),collapse=","),")")))

  ### set up forest plot (with 2x2 table counts added; rows argument is used
  ### to specify exactly in which rows the outcomes will be plotted)
  forest.new(res,ci.lb=ci.lb,ci.ub=ci.ub,slab=slab, xlim=c(low.all, high.all),digits=digits,
             #ilab=cbind(dat.bcg$tpos, dat.bcg$tneg, dat.bcg$cpos, dat.bcg$cneg),
             #ilab.xpos=c(-9.5,-8,-6,-4.5),
             cex=.75, ylim=c(0, nrows+1),psize=1, alim=alim,
             refline=refline,
             #order=order(ordervar,decreasing=T),
             rows=rows,
             xlab=xlab,
             refcol=refcol,
             panel.first=graphics::polygon(poly.x,poly.y,col=col,border=NA),at=at,at.lab=at.lab,
             ci.correct=ci.correct,scale.override=scale.override,...)

  if(is.null(wgts)) wgts = rep(1,nvars)

  var.pos <- high.plot+2*wd2*(wgts*(1:nvars)*nvars/2/(nvars+1))*slabchar*range.plot
  if(!is.null(pvals)) for(i in 1:nvars) add.var(vars[,i],var.pos[i],rows=rows,varlist[i] %in% pvals,bold.col=bold.col,highbold=highbold,show=rev(show),nobold=nobold,trendthresh=trendthresh,trend.col=trend.col,na.action=na.action,pdig=pdig)

  ### set font expansion factor (as in forest() above) and use bold italic
  ### font and save original settings in object 'op'
  op <- graphics::par(cex=.75, font=4)

  ### add text for the subgroups
  graphics::text(low.all, row2+1, pos=4, olab)

  ### switch to bold font
  graphics::par(font=2)

  ### add column headings to the plot
  graphics::text(mean(c(low.plot,high.plot)), nrows, graph.lab)
  graphics::text(low.all, nrows, col1, pos=4)
  graphics::text(var.pos,nrows,varlabs)
  if(is.null(ci.lab)) graphics::text(high.all-ci.correct, nrows, "Estimate [95% CI]", pos=2) else graphics::text(high.all-ci.correct, nrows, paste(ci.lab,"[95% CI]"), pos=2)

  makeFootnote(footnote)
  ### set par back to the original settings
  graphics::par(op)
  if(!is.null(na.action)) options(na.action=old.na.action)
  ### Draw a box around everything that won't get cropped by latex
  if(latex.out) grid::grid.polygon(x=c(0.005,0.005,0.995,0.995),y=c(0.005,0.995,0.995,0.005))
}
