\name{add.data.to.tips}
\alias{add.data.to.tips}
\title{
Adds data to the tip labels of a tree using a standard formula
}
\description{
This function is useful for adding data from one or more columns of a matrix to the tip labels of a tree
}
\usage{
add.data.to.tips(tr, datMat, delim = "[_|]", returnNum = 1:2, returnDelim = "_",
addCols = c("GROUP"), uniques = F, addDelim = "|", reorderTree = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{
    A \code{phylo} object
}
  \item{datMat}{
    A matrix with row names equal to the tips you want to match in the tree, after passing
    the tip labels through \link{label.elements}, and columns you want to add indicated
    using \code{addCols}
}
  \item{delim}{
    Delimiter for elements of the tip labels in the tree passed to the function, which
    will be used by \link{label.elements} to split the tip labels
}
  \item{returnNum}{
    The delimited elements of the tree tip labels that you want back from \link{label.elements}
}
  \item{returnDelim}{
    The delimiter to use in rebuilding the tip labels in \link{label.elements}
}
  \item{addCols}{
    Which columns of \code{datMat} to add to the new tip labels
}
  \item{uniques}{
    A boolean: if \code{TRUE}, any duplicate tip labels after passing through \link{label.elements}
    will be deleted
}
  \item{addDelim}{
    The delimiter to go between the tip label and the elements being added
}
  \item{reorderTree}{
    A boolean: if \code{TRUE}, the tree will be reordered so that the tip labels are in the \code{plot.phylo}
    order
}
  \item{\dots}{
    Additional arguments passed along to \link{label.elements}
}
}
\details{
}
\value{
  A list:
    \item{tr.relabelled }{An object of class \code{phylo}, relabelled}
    \item{labelMat }{An object of class \code{matrix} with original labels, the
    tips matched, the new elements added, the new labels, and whether the
    tip was retained or not}
}
\author{
  Andrew Hipp
}
\seealso{
  \code{\link{label.elements}}
  \code{\link{color.tips.by.element}}
  \code{\link{section.coloring}}
  \code{\link{tidyName}}
}
\examples{
}
\keyword{manip}
