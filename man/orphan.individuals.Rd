\name{orphan.individuals}
\alias{orphan.individuals}
\title{
Identify Vouchers Lacking Sequences
}
\description{
Determines which specimen vouchers do not have any sequences associated with them and which do.
}
\usage{
orphan.individuals(dat)
}
\arguments{
  \item{dat}{
A matrix, the output of \code{\link{make.gene.matrix}}.
}
}
\details{
Uses the function \code{\link{make.shared.gene.matrix}} internally.
All returned vectors are ordered to correspond with the rows of the gene matrix.
}
\value{
  \item{NCBI_accession}{A vector of pipe-delimited NCBI accession numbers for each voucher.}
  \item{taxon}{A vector of the taxon name of each voucher; if multiple, names are pipe-delimited.}
  \item{orphans}{A vector of all vouchers which do not have any gene sequences associated with them.}
  \item{notOrphans}{A vector of all vouchers which have at least one gene sequence associated with them.}
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
\code{\link{make.gene.matrix}}, \code{\link{make.shared.gene.matrix}}
}

\keyword{methods}
