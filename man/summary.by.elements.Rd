\name{summary.by.elements}
\alias{summary.by.elements}
\title{
Calculate Taxonomic Disparity For All Taxa
}
\description{
Calculate taxonomic disparity for all taxa represented in a tree,
assuming that tip labels are pipe-delimited and the first part of
the label is the taxon.
}
\usage{
summary.by.elements(tr, returnEffectOnTreeLength = TRUE, minSizeForEffect = 3, ...)
}
\arguments{
  \item{tr}{
  The tree to be analyzed, of class \code{phylo}
}
  \item{returnEffectOnTreeLength}{
  A binary determining whether each taxon's effect on tree length
  should be calculated and returned. Increases calculation time.
}
  \item{minSizeForEffect}{
  The minimum number of individuals in a taxon to calculate its
  effect on tree length.
}
  \item{\dots}{
  Arguments which can be passed to the function \code{label.elements}.
}
}
\details{
  The default for \code{returnEffectOnTreeLength} is \code{TRUE}.
  The default for \code{minSizeForEffect} is 3, and any taxa
  with less than 3 individuals is automatically assigned an
  effect size of 0.
}
\value{
  \item{disparity.mat }{A matrix with taxa as rows and the number of
  individuals in each taxon, the expected number of tips from the taxon's
  most recent common ancestor, and the taxonomic disparity of the taxon
  as columns.}
  \item{effectSize }{A vector of the effects of each taxon on the tree
  length. Names of each effect size are the taxa with which they are
  associated.}
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
\code{\link{label.elements}}, \code{\link{tips.expected}}
}

\keyword{ manip }
