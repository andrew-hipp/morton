\name{fas2bin}
\alias{fas2bin}
\title{
Convert a File in FASTA Format to Binary
}
\description{

}
\usage{
fas2bin(infile, outfile, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{infile}{
The address of the FASTA file to be converted.  If not in the current working directory, provide the full address.  The FASTA file must contain amino acid sequences, not DNA sequences.
}
  \item{outfile}{
The name (or full address, if necessary) of the output FASTA file to be written, containing the binary matrix.
}
  \item{\dots}{
Any other arguments that can be passed to the function \code{write.fasta} from the \code{ape} package.
}
}
\details{
This code makes use of the function \code{make.binary} from the same package.  See documentation of that function for more details on requirements for the binary matrix.
}
\value{
The converted matrix, where there can only be two states per column.
}
\author{
Andrew Hipp and Kasey Pham
}
\seealso{
\code{\link{make.binary}}, \code{\link{write.fasta}}
}

\keyword{ methods }
\keyword{ file }
