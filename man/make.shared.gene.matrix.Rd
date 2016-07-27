\name{make.shared.gene.matrix}
\alias{make.shared.gene.matrix}
\title{
Count Shared Specimens Between Loci
}
\description{
Using the output of make.gene.matrix, creates a gene matrix which summarizes 
the number of specimens shared between all pairs of gene loci.
}
\usage{
make.shared.gene.matrix(dat, loci = dimnames(dat)[[2]][!dimnames(dat)[[2]] \%in\% c("orgs", "ncbiAcc", "numberOfOrgs", "numberOfAccessions", "numberOfSequences")])
}
\arguments{
  \item{dat}{
The output of \code{make.gene.matrix}, a matrix linking gene sequence, specimen voucher, and gene locus.
}
  \item{loci}{
An optional vector of strings, the names of loci to include when counting shared specimens between pairs.
}
}
\details{
\code{loci} includes all loci by default.
}
\value{
A matrix of numeric values, where both rows and columns are loci 
and cells contain the number of individuals shared between two loci.}
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
\code{\link{make.gene.matrix}} 
}
\keyword{manip}
\keyword{methods}