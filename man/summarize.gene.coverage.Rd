\name{summarize.gene.coverage}
\alias{summarize.gene.coverage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summarize.gene.coverage(datMat, extra.left = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datMat}{
%%     ~~Describe \code{datMat} here~~
}
  \item{extra.left}{
%%     ~~Describe \code{extra.left} here~~
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
function (datMat, extra.left = 2, ...) 
{
    genes.no = dim(datMat)[2]
    inds.no = dim(datMat)[1]
    par(mar = c(5, 4 + extra.left, 4, 2) + 0.1)
    plot(0, type = "n", xlim = c(1, inds.no), ylim = c(0, genes.no + 
        1), xlab = "Individual sequenced [number]", ylab = "", 
        axes = FALSE)
    axis(2, at = seq(1:genes.no), labels = names(datMat), tick = FALSE, 
        las = 1)
    axis(1)
    for (i in 1:genes.no) points(which(datMat[1:inds.no, i] == 
        1), rep(i, sum(datMat[1:inds.no, i])), col = i, ...)
    return("done")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
