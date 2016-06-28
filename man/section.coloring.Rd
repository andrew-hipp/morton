\name{section.coloring}
\alias{section.coloring}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
section.coloring(tr, tipChar = "-", tip.cex = 0.1, tiplty = 0, 
                 pdfTitle = paste("trial.", paste(sample(letters, 3), collapse = ""), ".pdf", sep = ""), 
				 dist.cats = rep(1, length(tr$tip.label)), whiteOut = "NA", 
				 xy.multiplier = 1.5, offset.proportion = 0.03, writeLabels = 0.1, 
				 vectorToColorBy = label.elements(tr, "|", returnNum = 6, fixed = T),
				 ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{
%%     ~~Describe \code{tr} here~~
}
  \item{tipChar}{
%%     ~~Describe \code{tipChar} here~~
}
  \item{tip.cex}{
%%     ~~Describe \code{tip.cex} here~~
}
  \item{tiplty}{
%%     ~~Describe \code{tiplty} here~~
}
  \item{pdfTitle}{
%%     ~~Describe \code{pdfTitle} here~~
}
  \item{dist.cats}{
%%     ~~Describe \code{dist.cats} here~~
}
  \item{whiteOut}{
%%     ~~Describe \code{whiteOut} here~~
}
  \item{xy.multiplier}{
%%     ~~Describe \code{xy.multiplier} here~~
}
  \item{offset.proportion}{
%%     ~~Describe \code{offset.proportion} here~~
}
  \item{writeLabels}{
%%     ~~Describe \code{writeLabels} here~~
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
function (tr, tipChar = "-", tip.cex = 0.1, tiplty = 0, pdfTitle = paste("trial.", 
    paste(sample(letters, 3), collapse = ""), ".pdf", sep = ""), 
    dist.cats = rep(1, length(tr$tip.label)), whiteOut = "NA", 
    xy.multiplier = 1.5, offset.proportion = 0.03, writeLabels = 0.1, 
    ...) 
{
    tr <- read.tree(text = write.tree(tr))
    vectorToColorBy <- label.elements(tr, "|", returnNum = 6, 
        fixed = T)
    colors <- colors()[as.factor(vectorToColorBy)]
    colors[vectorToColorBy \%in\% whiteOut] <- "white"
    offset.levels <- dist.cats[vectorToColorBy]
    offset.levels[is.na(offset.levels)] <- 0
    tr$tip.label = rep(tipChar, length(tr$tip.label))
    a = plot(tr, "fan", tip.color = colors, align.tip.label = tiplty, 
        plot = FALSE, ...)
    offset.levels <- offset.levels * offset.proportion * abs(diff(a$x.lim))
    if (!is.na(pdfTitle)) 
        pdf(pdfTitle)
    a = plot(tr, "fan", tip.color = colors, align.tip.label = tiplty, 
        x.lim = a$x.lim * xy.multiplier, y.lim = a$y.lim * xy.multiplier, 
        label.offset = offset.levels, ...)
    if (writeLabels > 0) {
        unique.sections <- unique(vectorToColorBy)
    }
    if (!is.na(pdfTitle)) 
        dev.off()
    return(a)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
