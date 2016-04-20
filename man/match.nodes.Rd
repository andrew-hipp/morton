\name{match.nodes}
\alias{match.nodes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
match.nodes(guideTree, otherTrees, matchBoots = T, plotBoots = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{guideTree}{
%%     ~~Describe \code{guideTree} here~~
}
  \item{otherTrees}{
%%     ~~Describe \code{otherTrees} here~~
}
  \item{matchBoots}{
%%     ~~Describe \code{matchBoots} here~~
}
  \item{plotBoots}{
%%     ~~Describe \code{plotBoots} here~~
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
function (guideTree, otherTrees, matchBoots = T, plotBoots = T, 
    ...) 
{
    if ("phylo" \%in\% class(otherTrees)) 
        otherTrees = list(tr1 = otherTrees)
    matched <- lapply(otherTrees, function(x) matchNodes(guideTree, 
        x))
    match.mat <- do.call(cbind, c(guideTree = list(matched[[1]][, 
        1]), lapply(matched, function(x) x[, 2])))
    match.index <- which(apply(match.mat, 1, function(x) !any(is.na(x))))
    if (matchBoots) {
        allTreesLabels <- lapply(c(guideTree = list(guideTree), 
            otherTrees), function(x) c(x$tip.label, x$node.label))
        mat.boots <- match.mat[match.index, ]
        for (i in seq(dim(mat.boots)[2])) mat.boots[, i] <- as.integer(allTreesLabels[[i]][mat.boots[, 
            i]])
        out <- list(mat.boots = mat.boots, mat.full = match.mat, 
            mat.index = match.index)
        if (plotBoots) 
            matplot(mat.boots[order(its.ets.matched$mat.boots[, 
                1], decreasing = T), ], ...)
    }
    else out <- list(mat.full = match.mat, mat.index = match.index)
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
