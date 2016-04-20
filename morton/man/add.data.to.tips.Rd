\name{add.data.to.tips}
\alias{add.data.to.tips}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
add.data.to.tips(tr, datMat, delim = "[_|]", returnNum = 1:2, returnDelim = "_", addCols = c("GROUP"), uniques = F, addDelim = "|", reorderTree = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{
%%     ~~Describe \code{tr} here~~
}
  \item{datMat}{
%%     ~~Describe \code{datMat} here~~
}
  \item{delim}{
%%     ~~Describe \code{delim} here~~
}
  \item{returnNum}{
%%     ~~Describe \code{returnNum} here~~
}
  \item{returnDelim}{
%%     ~~Describe \code{returnDelim} here~~
}
  \item{addCols}{
%%     ~~Describe \code{addCols} here~~
}
  \item{uniques}{
%%     ~~Describe \code{uniques} here~~
}
  \item{addDelim}{
%%     ~~Describe \code{addDelim} here~~
}
  \item{reorderTree}{
%%     ~~Describe \code{reorderTree} here~~
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
function (tr, datMat, delim = "[_|]", returnNum = 1:2, returnDelim = "_", 
    addCols = c("GROUP"), uniques = F, addDelim = "|", reorderTree = TRUE, 
    ...) 
{
    if (reorderTree) 
        tr <- read.tree(text = write.tree(tr))
    tips.to.match <- label.elements(tr, delim, returnNum, returnDelim, 
        ...)
    if (uniques) 
        tips.to.drop <- which(duplicated(tips.to.match))
    addVect <- datMat[match(tips.to.match, row.names(datMat)), 
        addCols]
    if (!is.null(dim(addVect))) 
        addVect <- apply(addVect, 1, paste, collapse = addDelim)
    oldNames <- tr$tip.label
    newLabel <- paste(oldNames, addVect, sep = addDelim)
    tr$tip.label <- newLabel
    if (uniques) 
        tr <- drop.tip(tr, tr$tip.label[tips.to.drop])
    out = list(tr.relabelled = tr, labelMat = cbind(oldLabel = oldNames, 
        tipMatched = tips.to.match, newElement = addVect, newLabel = newLabel, 
        retained = !duplicated(tips.to.match)))
    row.names(out$labelMat) <- out$labelMat[, "newLabel"]
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
