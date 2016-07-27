\name{refine.fasta}
\alias{refine.fasta}
\title{
Clean Up FASTA Files to User Specification
}
\description{
Reverse-complement or delete sequences in a multiple sequence alignment based 
on a csv file input.
}
\usage{
refine.fasta(basedir = choose.dir())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{basedir}{
The address of the directory which contains the FASTA files to be refined and 
corresponding log files.
}
}
\details{
\code{make.fasta.files} automates the export of FASTA and log files. 
A separate FASTA file and corresponding log file is exported for each
locus included in the analysis.

Any log files used in this function must match the format of those exported 
by \code{make.fasta.files}.
Log files must be in csv format, and have the following columns:
  - orgs
  - seqLabels
  - whatToDo
  - comments
\code{orgs} is the taxon name of the specimen. \code{seqLabels} is the 
label of the sequence in the corresponding alignment. \code{whatToDo} 
has three possible states depending on what needs to be done to a 
sequence in the alignment. 
  - \code{RC} = reverse complement
  - \code{D} = delete
  - blank = do nothing
Log files start blank when exported. The user should manually edit 
log files to add these values before running \code{refine.fasta}.
}
\value{
Returns the string 'done!' when fasta files have been imported and
processed. Function invoked for side-effect.
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
 \code{\link{make.gene.matrix}}, \code{\link{parse.INSDSeq}}
}

\keyword{methods}
\keyword{file}
