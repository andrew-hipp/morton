\name{parse.INSDSeq}
\alias{parse.INSDSeq}
\title{
Parse Raw Gene Database Data from XML File
}
\description{
Parse raw XML file downloaded from a gene database such as NCBI Genbank, containing sequence data and its accompanying metadata, ie: NCBI accession number and collection information about the sample from which the sequence came.  Parsed sequences and metadata are outputted in a matrix.
}
\usage{
parse.INSDSeq(xml_file, do = NA, includeSeqs = F, cores = 1, parse.specimens = T, qualsToUse = c("specimen_voucher", "country", "collection_date", "lat_lon", "note", "collected_by", "isolate", "pop_variant"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xml_file}{
The raw XML file to be parsed, already read into R as an object using WRITE MORE HERE.
}
  \item{do}{
WRITE MORE HERE
}
  \item{includeSeqs}{
An optional logical value indicating whether sequences should be written into the output (TRUE) or ignored during parsing (FALSE).
}
  \item{cores}{
Optional, the number of cores to use.  Multithreading is only supported for Linux and MacOS builds.
}
  \item{parse.specimens}{
An optional logical value indicating whether the specimen field associated with each sequence should be parsed and written to output.
}
  \item{qualsToUse}{
A vector of the categories of metadata to be parsed from the XML data and included in the output.
}
}
\details{
The default value for \code{do} is \code{NA}.
The default value for \code{includeSeqs} is \code{FALSE}.
The default value for \code{cores} is 1.
The default value for \code{parse.specimens} is \code{TRUE}.
The default for \code{qualsToUse} is \code{c('specimen_voucher', 'country', 'collection_date', 'lat_lon', 'note', 'collected_by', 'isolate', 'pop_variant')}.
}
\value{
A matrix containing all parsed information, where each row contains the information associated with a single gene database entry. Columns are the categories of information parsed from the data.
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
\code{\link{parse.specimen}}, WRITE MORE HERE: MULTICORE SUPPORT
}

\keyword{ manip }
