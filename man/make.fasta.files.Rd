\name{make.fasta.files}
\alias{make.fasta.files}
\title{
Export Loci in FASTA Format
}
\description{
Write a separate FASTA file for all loci specified to an output directory given sequence data and a matrix linking accession numbers to specimen vouchers and loci.
}
\usage{
function(geneMatrix, seqDat, genes,
           outdir = paste('fasta.out.',
                          paste(sample(letters,5), collapse = ''),
                          format(Sys.time(), '.%Y-%m-%d'),
                          sep = ''),
           maxtax = 1, treat.multiples = c('discard', 'takefirst'),
           batchfile = 'muscle')
}
\arguments{
  \item{geneMatrix}{
The output from \code{make.gene.matrix}.  Connects vouchers with gene sequences and gene regions.
}
  \item{seqDat}{
The output of \code{parse.INSDSeq}.  A data frame where each row contains a sequence and its associated metadata.
}
  \item{genes}{
A vector containing the names of all gene regions to be written, each to its own separate FASTA file.  Should correspond to names of columns of geneMatrix.
}
  \item{outdir}{
An optional string, the name of the directory to which the FASTA files should be written.  Will be created in current working directory if it does not already exist.
}
  \item{maxtax}{
An optional integer, the maximum number of taxon names allowable per individual.
}
  \item{treat.multiples}{
An optional vector containing options for treatment of individuals with multiple NCBI accession numbers for a given gene; either discards the voucher for the region or uses the first accession number only.
}
  \item{batchfile}{
An optional string, the name of the program that will process the FASTA files.
}
}
\details{
The default for \code{outdir} is the current working directory.
The default for \code{maxtax} is 1.
The default for \code{treat.multiples} is to discard.
The default for \code{batchfile} is to export a batchfile invoking the program muscle. With this option, a directory will be created in the current working directory for storing the exported FASTA files and the batch file. Within that directory, another will be created to store processed muscle alignments. The batch file will call muscle to use this doubly-nested directory for its outputs.
}
\value{
Returns the string done! Called for the side-effect of writing fasta files to disk.
}
\references{
Edgar, Robert C. (2004), MUSCLE: multiple sequence alignment with high accuracy and high throughput, Nucleic Acids Research 32(5), 1792-97.
}
\author{
Kasey Pham and Andrew Hipp
}

\seealso{
\code{\link{make.gene.matrix}}, \code{\link{parse.INSDSeq}}
}
\keyword{IO}
\keyword{file}
\keyword{methods}
