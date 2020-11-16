\name{tips.expected}
\alias{tips.expected}
\title{
Calculate Expected Tips in a Clade
}
\description{
Given a tree and a set of tips, this function determines the most recent
common ancestor (MRCA) of these tips and calculates how many tips are 
expected to be in a clade with this MRCA. Requires the installation of
the package \code{phangorn}.
}
\usage{
tips.expected(tr, tips, value = FALSE, ...)
}
\arguments{
  \item{tr}{
  The tree, of class \code{phylo}.
}
  \item{tips}{
  A vector of tip labels from which to calculate MRCA.
}
  \item{value}{
  A boolean determining what the function returns. If \code{TRUE},
  it returns a vector of the tip labels of the MRCA's
  expected descendents for a full clade. If \code{FALSE}, it returns 
  the number of expected descendents.
}
  \item{\dots}{
  Arguments to be passed to the function \code{Descendents} in
  phangorn.
}
}
\details{
  The default for value is \code{FALSE}.
}
\value{
  \item{out }{Either the number of expected descendents from the MRCA,
  or a vector of tip labels of the expected descendents, depending on
  the argument \code{value}.}
}
\references{
Schliep K.P. 2011. phangorn: phylogenetic analysis in R. Bioinformatics, 27(4) 592-593. http://phangorn.org.
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
\code{\link{phangorn}}, \code{\link{getMRCA}}, \code{\link{Descendents}}
}

\keyword{ manip }
