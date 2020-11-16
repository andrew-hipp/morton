\name{color.tips.by.element}
\alias{color.tips.by.element}
\title{
  Color the tip labels of a tree according to elements of the tip labels
}
\description{
Takes a tree with tip labels structured in a consistent manner, and colors according to unique elements
}
\usage{
color.tips.by.element(tr, element = 6, delim = "|", fixed = TRUE, whiteOut = "NA", addLegend = T,
colorIt = FALSE, byLabels = TRUE, tip.cex = 0.1, dot.pch = 1, blank.tips = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{
 a \code{phylo} object
}
  \item{element}{
 which element of the delimited labels to use for coloring
}
  \item{delim}{
 label delimters
}
  \item{fixed}{
 boolean: should \code{strsplit} use \code{fixed = TRUE} or not
}
  \item{whiteOut}{
 a character vector identifying labels that should be whited out; defaults to \code{NA}
}
  \item{addLegend}{
 boolean
}
  \item{colorIt}{
 boolean: if \code{TRUE}, colored dots are added at the tips 
}
  \item{byLabels}{
 boolean: if \code{TRUE}, tip labels are reassigned the label element, and colored
}
  \item{tip.cex}{
 \code{cex} for tip labels
}
  \item{dot.pch}{
 character code for \code{colorIt} option
}
  \item{blank.tips}{
 boolean: if \code{TRUE}, tip labels are not plotted
}
  \item{\dots}{
 additional arguments to \code{plot.phylo}
}
}
\details{
The default assumes tips are pipe-delimited. If tips are species|country|subcountry|collectorNumber, for example, 
use \code{element = 2} to color by country.
}
\value{
the \code{plot.phylo} value, returned invisibly
}
\author{
Andrew Hipp
}
\seealso{
\code{\link{section.coloring}}
\code{\link{label.elements}}
}
