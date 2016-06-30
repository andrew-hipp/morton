\name{orphan.individuals}
\alias{orphan.individuals}
\title{
Identify Vouchers Lacking Sequences
}
\description{
Determines which specimen vouchers do not have any sequences associated with them and which do.
}
\usage{
orphan.individuals(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
A matrix, the output of \code{\link{make.gene.matrix}}.
}
}
\details{
Uses the function \code{\link{make.shared.gene.matrix}} internally.
}
\value{
  \item{NCBI_accession}{A vector of all NCBI accession numbers}
  \item{taxon}{Description of 'comp2'}
  \item{orphans}{}
  \item{notOrphans}{}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
\code{\link{make.gene.matrix}}, \code{\link{make.shared.gene.matrix}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dat) 
{
    shared.loci <- make.shared.gene.matrix(dat)
    not.orphan.loci <- names(which(colSums(shared.loci) > 0))
    dat.no.orphan.loci <- dat[, not.orphan.loci]
    not.orphan.individuals <- apply(dat.no.orphan.loci, 1, function(x) sum(x) > 
        0)
    orphan.individuals <- apply(dat.no.orphan.loci, 1, function(x) sum(x) == 
        0)
    out <- list(NCBI_accession = dat$ncbiAcc, taxon = dat$orgs, 
        orphans = orphan.individuals, notOrphans = not.orphan.individuals)
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
