\name{monophyletic.spp}
\alias{monophyletic.spp}
\title{
Summary of Species Coverage and Monophyly
}
\description{
Generate summary of how many species there are and whether they are monophyletic from a tree of class \code{phylo}.
}
\usage{
monophyletic.spp(tree, ...)
}
\arguments{
  \item{tree}{
The tree to be summarized.
}
  \item{\dots}{
Any other arguments besides the tree itself passable to the function \code{label.elements}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{numSpp }{The number of unique species in the tree.}
  \item{spp.summary }{A matrix where unique species are the rows, and the columns are
                        1. A count of how many tips of each species there are,
                        2. Logical values indicating whether or not the species is monophyletic in the tree given.
                        3. The consistency index for each species.
}
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
\code{\link{label.elements}}, \code{\link{is.monophyletic}}, \code{\link{tips.ci}}
}

\keyword{ methods }
