\name{make.shared.gene.matrix}
\alias{make.shared.gene.matrix}
\title{
Count Shared Specimens Between Loci
}
\description{
Using the output of make.gene.matrix, creates a gene matrix which summarizes 
the number of specimens shared between all pairs of gene loci.
}
\usage{
make.shared.gene.matrix(dat, loci = dimnames(dat)[[2]][!dimnames(dat)[[2]] \%in\% c("orgs", "ncbiAcc", "numberOfOrgs", "numberOfAccessions", "numberOfSequences")])
}
\arguments{
  \item{dat}{
The output of \code{\link{make.gene.matrix}}, a matrix linking gene sequence, specimen voucher, and gene locus.
}
  \item{loci}{
An optional vector of strings, the names of loci to include when counting shared specimens between pairs.
}
}
\details{
\code{loci} includes all loci by default.
}
\value{
A matrix of numeric values, where both rows and columns are loci 
and cells contain the number of individuals shared between two loci.}
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
\code{\link{make.gene.matrix}} 
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dat, loci = dimnames(dat)[[2]][!dimnames(dat)[[2]] \%in\% 
    c("orgs", "ncbiAcc", "numberOfOrgs", "numberOfAccessions", 
        "numberOfSequences")]) 
{
    out <- matrix(NA, length(loci), length(loci), dimnames = list(loci, 
        loci))
    for (i in 1:length(loci)) {
        for (j in 1:length(loci)) {
            out[loci[i], loci[j]] <- sum(ifelse(dat[[loci[i]]] == 
                0, 0, 1) + ifelse(dat[[loci[j]]] == 0, 0, 1) == 
                2)
        }
    }
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{methods}