\name{make.binary}
\alias{make.binary}
\title{
Convert a Character Matrix to a Binary Character Matrix
}
\description{
Convert a matrix of characters into a binary matrix by removing all columns that do not contain characters in exactly 2 states.
}
\usage{
make.binary(dat, binChars = strsplit("ACDEFGHIKLMNPQRSTVWY", "")[[1]], extras = c("-"))
}
\arguments{
  \item{dat}{
A matrix of characters.
}
  \item{binChars}{
An optional vector of all acceptable characters to represent a state in the binary matrix.
}
  \item{extras}{
An optional vector of all other acceptable characters which can be present in the binary matrix. These will not be counted towards one state or another.
}
}
\details{
The default for binChars is all \code{c('A','C','D','E','F','G','H','I','K','L','M','N','P','Q','R','S','T','V','W','Y')}, the IUPAC one-letter abbreviations for amino acids.

The default for extras is \code{c('-')}, the symbol for a gap in an alignment.

This function works by requiring that each column in a matrix have exactly two character states that have been listed as acceptable in \code{binChars}, or an acceptable non-state listed in \code{extras}. It removes all columns that do not fit these criteria and returns the resulting matrix.
}
\value{
The converted character binary matrix.
}
\author{
Kasey Pham and Andrew Hipp
}
\seealso{
\code{\link{fas2bin}}
}

\keyword{ methods }
\keyword{ manip }
