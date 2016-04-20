\name{color.tips.by.element}
\alias{color.tips.by.element}
\title{
  Color the tip labels of a tree according to elements of the tip labels
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
color.tips.by.element(tr, element = 6, delim = "|", fixed = TRUE, whiteOut = "NA", addLegend = T,
colorIt = FALSE, byLabels = TRUE, tip.cex = 0.1, dot.pch = 1, blank.tips = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{
%%     ~~Describe \code{tr} here~~
}
  \item{element}{
%%     ~~Describe \code{element} here~~
}
  \item{delim}{
%%     ~~Describe \code{delim} here~~
}
  \item{fixed}{
%%     ~~Describe \code{fixed} here~~
}
  \item{whiteOut}{
%%     ~~Describe \code{whiteOut} here~~
}
  \item{addLegend}{
%%     ~~Describe \code{addLegend} here~~
}
  \item{colorIt}{
%%     ~~Describe \code{colorIt} here~~
}
  \item{byLabels}{
%%     ~~Describe \code{byLabels} here~~
}
  \item{tip.cex}{
%%     ~~Describe \code{tip.cex} here~~
}
  \item{dot.pch}{
%%     ~~Describe \code{dot.pch} here~~
}
  \item{blank.tips}{
%%     ~~Describe \code{blank.tips} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (tr, element = 6, delim = "|", fixed = TRUE, whiteOut = "NA",
    addLegend = T, colorIt = FALSE, byLabels = TRUE, tip.cex = 0.1,
    dot.pch = 1, blank.tips = T, ...)
{
    vectorToColorBy <- label.elements(tr, delim, returnNum = element,
        fixed = fixed)
    colors = colors()[as.factor(vectorToColorBy)]
    colors[vectorToColorBy \%in\% whiteOut] <- "white"
    par(mar = c(5, 10, 4, 2))
    if (blank.tips)
        tr$tip.label <- sapply(tr$tip.label, function(x) "")
    a = plot(tr, ...)
    if (colorIt)
        tiplabels(col = colors, pch = dot.pch, cex = tip.cex)
    if (byLabels)
        tiplabels(vectorToColorBy, cex = tip.cex, align.tip.label = T)
    if (addLegend)
        legend(a$x.lim[1] - abs(diff(a$x.lim)/4), a$y.lim[2],
            legend = unique(vectorToColorBy), pch = dot.pch,
            cex = 1, col = unique(colors), bty = "n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
