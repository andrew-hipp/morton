\name{label.elements}
\alias{label.elements}
\title{
Extract Information from Tip Labels of a Phylogenetic Tree
}
\description{
Parse tip labels of an object of class \code{phylo} and
store information to a vector with the same order as
the tip labels of the tree.
}
\usage{
label.elements(x, delim = "|", returnNum = 1, returnDelim = " ", ...)
}
\arguments{
  \item{x}{
The tree whose tips are to be parsed. Should be of class \code{phylo}.
}
  \item{delim}{
The symbol in the tip labels used to delimit different pieces of
information.
}
  \item{returnNum}{
The place of the information desired from the tip labels. For example,
if information is delimited by |, species is desired, and tips are in
the following format:
  Quercus_rubra|ARB355|Johnson
then the returnNum would be 1, as the species is the first piece of
delimited information.
}
  \item{returnDelim}{
The delimitation character for the new tip labels generated.
}
  \item{\dots}{
Other arguments pertinent to the function \code{\link{strsplit}}.
}
}
\details{
The default value for \code{delim} is a pipe (|). The default for
returnNum is 1, and the default for returnDelim is a space, ' '.
}
\value{
  \item{out}{A vector of the information extracted from the tip
  labels of the tree, ordered corresponding to tip labels.}
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
\code{\link{strsplit}}
}
\keyword{methods}
