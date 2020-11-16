\name{section.coloring}
\alias{section.coloring}
\title{
Color Tree Tips by Section
}
\description{
Plot a circular tree with tips colored by section to a PDF file. Assumes that
section is the sixth pipe-delimited part of a tip label.
}
\usage{
section.coloring(tr, tipChar = "-", tiplty = 0, 
                 pdfTitle = paste("trial.", paste(sample(letters, 3), collapse = ""), ".pdf", sep = ""), 
				 dist.cats = rep(1, length(tr$tip.label)), whiteOut = "NA", 
				 xy.multiplier = 1.5, offset.proportion = 0.03, writeLabels = 0.1, 
				 vectorToColorBy = label.elements(tr, "|", returnNum = 6, fixed = T),
				 ...)
}
\arguments{
  \item{tr}{
  The tree to be plotted, an object of class \code{phylo}.
}
  \item{tipChar}{
  Optional. The character with which to replace the full tip label.
}
  \item{tiplty}{
  
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

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
