\name{make.unique.vouchers}
\alias{make.unique.vouchers}
\title{
Assign Specimen Vouchers to Gene Sequences
}
\description{
Generates a vector of vouchers with order corresponding to order of sequences 
in rows of provided metadata table. 
}
\usage{
make.unique.vouchers(metadata, voucherFormula = c("Primary.collector.last.name", "Collector.number", "isolate", "CollectionNumber", "Collection"))
}
\arguments{
  \item{metadata}{
A data frame of sequence-associated metadata, the output of \code{parse.INSDSeq}.
}
  \item{voucherFormula}{
A vector of strings, the names of the columns in the metadata that are to be 
included in the voucher.
}
}
\details{
The default for voucherFormula is \code{Primary.collector.last.name, Collector.number, isolate, CollectionNumber, Collection},
which correspond to default column names in the metadata. \code{organism} is not included in this 
formula, as specimens can be re-classified throughout their lifetime, so taxon name may not stay
constant across all sequences belonging to a single specimen.

The function \code{tidyName} is used internally to standardize vouchers so that differences in capitalization and punctuation do not prevent the linkage of two gene sequences to a single specimen.

The output of this function should be added as a column to the metadata data frame in order to
use the \code{make.gene.matrix} function. This can be done with \code{cbind}.
}
\value{
A vector of strings, containing a generated voucher for each 
sequence, order corresponding to the rows of the metadata.}
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
\code{\link{parse.INSDSeq}}, \code{\link{tidyName}}
}
\keyword{manip}
