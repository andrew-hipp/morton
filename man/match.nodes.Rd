\name{match.nodes}
\alias{match.nodes}
\title{
Match and Plot Nodes of Multiple Trees
}
\description{
Match the nodes and bootstraps of multiple trees to one reference tree and plot bootstrap values.
}
\usage{
match.nodes(guideTree, otherTrees, matchBoots = T, plotBoots = T, ...)
}
\arguments{
  \item{guideTree}{
The reference tree to which other tree's nodes and bootstraps are matched.
}
  \item{otherTrees}{
A single tree (class \code{phylo}) or a list of trees to be matched to the reference tree.
}
  \item{matchBoots}{
An optional logical value -- whether the function should also match bootstraps in addition to nodes.
}
  \item{plotBoots}{
An optional logical value-- whether the function should plot bootstrap values.
}
  \item{\dots}{
Other arguments that can be passed on to the function \code{matplot}.
}
}
\details{
\code{matchBoots} and \code{plotBoots} both default to \code{TRUE}.
}
\value{
  \item{mat.full}{A matrix of all matched nodes.}
  \item{mat.index}{all rows of \code{mat.full} that contain values.}
}
\author{
Andrew Hipp and Kasey Pham
}
\seealso{
\code{\link{matchNodes}}, \code{\link{matplot}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }