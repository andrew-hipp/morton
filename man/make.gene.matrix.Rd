\name{make.gene.matrix}
\alias{make.gene.matrix}
\title{
Linkage of Specimens to Sequences for Multiple Loci
}
\description{
Creates a matrix which links gene sequences from multiple loci to their associated specimen, determined by assigning a voucher to each specimen using parsed metadata for each sequence.
}
\usage{
make.gene.matrix(metadata, locusCol = "cleanedGeneRegion", vouchersCol = "newLabels", ncbiCol = "NCBI_accession", orgsCol = "organism", logerrors = TRUE, verbose = FALSE)
}
\arguments{
  \item{metadata}{
The output of \code{parse.INSDSeq}, containing all parsed metadata and sequences from the raw XML data. Must contain a column for vouchers, which can be made with \code{make.unique.vouchers.R} and added to metadata data frame using \code{cbind}.
}
  \item{locusCol}{
An optional string, the name of the column in the metadata which contains the name of the gene region.
}
  \item{vouchersCol}{
An optional string, the name of the column in the metadata which contains the voucher.
}
  \item{ncbiCol}{
An optional string, the name of the column in the metadata which contains the NCBI accession number.
}
  \item{orgsCol}{
An optional string, the name of the column in the metadata which contains the name of the taxon.
}
  \item{logerrors}{
An optional logical value indicating whether the function should export a csv file with sequences which did not have a voucher, which are automatically excluded from the output matrix.
  \item{verbose}{
An optional logical value indicating whether the function should print every row of the metadata it successfully incorporates into the matrix.
}
}
\details{
\code{locusCol}, \code{vouchersCol}, \code{ncbiCol}, and \code{orgsCol} all 
have default values that correspond to the default names of those columns from 
other functions in the morton package. They are \code{cleanedGeneRegion}, 
\code{newLabels}, \code{NCBI_accession}, and \code{organism}, respectively.
  
The default value for \code{logerrors} is TRUE. For \code{verbose}, it is FALSE.

\code{verbose} can be a useful tool when troubleshooting to pinpoint where the 
function has stopped.

This function traverses the metadata data table from \code{parse.INSDSeq} and 
generates a matrix where unique vouchers are the rows andgene loci are the 
columns. Each cell represents a sequence, where its x and y position in the 
matrix indicate which voucher and gene locus it belongs to. Cells therefore 
contain the NCBI accession number for the sequence which they are associated 
with. If there are multiple sequences for a single voucher and gene locus, the 
NCBI accession numbers are both entered into the cell, delimited by a pipe(|).
}
\value{
A matrix which connects NCBI gene sequences to their associated loci and vouchers.
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
\code{\link{parse.INSDSeq}}, \code{\link{make.unique.vouchers}}, \code{\link{make.fasta.files}}, \code{\link{make.shared.gene.matrix}}, \code{\link{cbind}}
}
\keyword{manip}
\keyword{methods}
