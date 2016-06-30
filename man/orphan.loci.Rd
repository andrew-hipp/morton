\name{orphan.loci}
\alias{orphan.loci}
\title{
Identify Gene Loci Lacking Sequences.
}
\description{
Identify all loci which do not have any gene sequences from the dataset associated with them.
}
\usage{
orphan.loci(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The output of \code{\link{make.shared.gene.matrix}}.
}
}
\value{
A vector of the names of all loci with no sequences.
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
\code{\link{make.shared.gene.matrix}}
}

\keyword{methods}