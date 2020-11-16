\name{summarize.gene.coverage}
\alias{summarize.gene.coverage}
\title{
Plot Individuals Sequenced Per Gene
}
\description{
Plot the number of individuals sequenced per gene given a gene matrix from
\code{make.gene.matrix}.
}
\usage{
summarize.gene.coverage(datMat, extra.left = 2, ...)
}
\arguments{
  \item{datMat}{
  A gene matrix from \code{make.gene.matrix}.
}
  \item{extra.left}{
  Optional. The amount of space to add to the left margin of the plot.
}
  \item{\dots}{
  Other arguments which can be passed to the function \code{points}.
}
}
\details{
  The default for \code{extra.left} is 2.
}
\value{
Returns the string 'done!'. Called for the side-effect of plotting to R graphics device.
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
\code{\link{make.gene.matrix}}, \code{\link{points}}
}

\keyword{ hplot }
