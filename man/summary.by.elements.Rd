\name{summary.by.elements}
\alias{summary.by.elements}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.by.elements(tr, returnEffectOnTreeLength = TRUE, minSizeForEffect = 3, recommendDrops = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{
%%     ~~Describe \code{tr} here~~
}
  \item{returnEffectOnTreeLength}{
%%     ~~Describe \code{returnEffectOnTreeLength} here~~
}
  \item{minSizeForEffect}{
%%     ~~Describe \code{minSizeForEffect} here~~
}
  \item{recommendDrops}{
%%     ~~Describe \code{recommendDrops} here~~
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
function (tr, returnEffectOnTreeLength = TRUE, minSizeForEffect = 3, 
    recommendDrops = TRUE, ...) 
{
    all.elements <- label.elements(tr, ...)
    unique.elements <- sort(unique(all.elements))
    out <- cbind(count = sapply(unique.elements, function(x) sum(all.elements == 
        x)), expected = sapply(unique.elements, function(x) tips.expected(tr, 
        names(all.elements)[all.elements == x])))
    out <- list(disparity.mat = cbind(out, disparity = out[, 
        "expected"] - out[, "count"]), effectSize = NA)
    if (returnEffectOnTreeLength) {
        message("calculating effect size... please be patient")
        effectOnTreeLength <- lapply(unique.elements, function(x) {
            if (out$disparity.mat[x, "count"] < minSizeForEffect) 
                return(0)
            message(paste("doing effect size for", x))
            tr.temp <- drop.tip(tr, tr$tip.label[which(all.elements != 
                x)])
            effectSizeByTaxon <- sum(tr.temp$edge.length) - sapply(tr.temp$tip.label, 
                function(y) sum(drop.tip(tr.temp, y)$edge.length))
            return(effectSizeByTaxon)
        })
        out$effectSize <- effectOnTreeLength
        names(out$effectSize) <- unique.elements
    }
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
