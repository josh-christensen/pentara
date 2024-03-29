% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sforest.R
\name{sforest}
\alias{sforest}
\title{Classic forest plot}
\usage{
sforest(
  dat,
  res,
  ci.lb,
  ci.ub,
  ordervar,
  orderlab,
  suborder,
  sublab,
  xlab,
  footnote = "",
  varlist = NULL,
  varlabs = NULL,
  pvals = NULL,
  refline = 0,
  col = "aliceblue",
  wd1 = 0.5,
  wd2 = 0.5,
  alim = NULL,
  col1 = " ",
  bold.col = "black",
  latex.out = TRUE,
  express = FALSE,
  expresso = FALSE,
  highbold = FALSE,
  digits = 2,
  refcol = "black",
  show = NULL,
  nobold = FALSE,
  wgts = NULL,
  trendthresh = 0.05,
  trend.col = "black",
  at = NULL,
  at.lab = NULL,
  ci.lab = NULL,
  na.action = NULL,
  pdig = 4,
  graph.lab = "Graphical Summary",
  ci.correct = 0,
  scale.override = NULL,
  ...
)
}
\arguments{
\item{dat}{data for the plot}

\item{res}{a string naming a column to plot as points}

\item{ci.lb}{a string naming column to plot as ci upper bounds}

\item{ci.ub}{a string naming column to plot as ci lower bounds}

\item{ordervar}{a string naming a column that defines the order of sections.
The column must be numeric and have the same value for all rows in the same
section.}

\item{orderlab}{a string naming a column of section labels. The column must
be character and have the same value for all rows in the same section.}

\item{suborder}{a string naming a column that defines the order within
sections. The column must be numeric and have different values for all rows
in the same section.}

\item{sublab}{a string naming a column of row labels. The column must be
character and have different values for all rows in the same section.}

\item{xlab}{a string to be used as the x-axis label}

\item{footnote}{a string to use as a footnote}

\item{varlist}{a character vector containing the names of other columns to be
included in the plot}

\item{varlabs}{a character vector containing the labels to be used in the
plot for variables in varlist}

\item{pvals}{a character vector containing the names of columns in varlist
that should be formatted as p-values}

\item{refline}{a numeric value indicating where the dashed vertical reference
line should be drawn}

\item{col}{color used for the background bands}

\item{wd1}{numeric adjustment for the space between the left text and the
plot}

\item{wd2}{numeric adjustment for the space between the right text and the
plot}

\item{alim}{adjust the x-axis limits for the plot}

\item{col1}{a string used to label the column of text on the left of the plot}

\item{bold.col}{color for bold p-values}

\item{latex.out}{draws only a box around what won't get cropped by latex}

\item{express}{logical. If TRUE, do not coerce sublab variable to character
and do not automatically indent}

\item{expresso}{logical. if TRUE, do not coerce orderlab variable to
character}

\item{highbold}{logical. Currently must be FALSE}

\item{digits}{numeric vector indicating how many digitis should be displayed
after the decimal palce in the estimate column and on the x-axis. If only
one number is provided it is used for both}

\item{refcol}{color for the reference line}

\item{show}{logical vector of the same length as the number of rows in the
plot indicating which p-values should be displayed}

\item{nobold}{logical. If TRUE do not bold significant p-values}

\item{wgts}{numeric vector that shifts added text columns multiplicatively
relative to the plot}

\item{trendthresh}{threshold for p-values identifying a trend}

\item{trend.col}{color to use in identifying p-values that show a trend}

\item{at}{vector of numeric values at which to place x-axis ticks}

\item{at.lab}{vector of labels for x-axis ticks. Must be used with at and
have the same length as at}

\item{ci.lab}{string used to label the default column in the form '\code{string}
[95\% CI]'}

\item{na.action}{na.action to be used within sforest}

\item{pdig}{number of digits to display for p-values}

\item{graph.lab}{label for the top of the plot}

\item{ci.correct}{number used to move the default CI column to the left}

\item{scale.override}{numeric vector of the same length as at.lab that
creates a new x-axis spanning the entire graphic. Rarely useful}

\item{...}{Further arguments to be passed to various subplotting functions}
}
\value{
invisibly returns NULL
}
\description{
Creates forest plot that matches Pentara's historic forest plot
style.
}
\examples{
forest_dat <- data.frame(
  mean = c(.1, .5, .2, .8),
  lower = c(.1, .5, .2, .8) - qnorm(.975),
  upper = c(.1, .5, .2, .8) + qnorm(.975),
  ordervar = rep(c(1,2), each = 2),
  orderlab = rep(c("Study 1", "Study 2"), each = 2),
  subordervar = rep(c(1,2), times = 2),
  suborderlab = rep(c("Endpoint 1", "Endpoint 2"), times = 2),
  p_val = c(.3756, .1023, .1542, .043)
)

sforest(
  dat = forest_dat,
  res = "mean",
  ci.lb = "lower",
  ci.ub = "upper",
  ordervar = "ordervar",
  orderlab = "orderlab",
  suborder = "subordervar",
  sublab = "suborderlab",
  varlist = c("p_val"),
  varlabs = c("P-Value"),
  pvals = c("p_val"),
  xlab = "Treatment Effect",
  latex.out = FALSE
)
}
