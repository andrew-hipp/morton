\name{orphan.individuals}
\alias{orphan.individuals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
orphan.individuals(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
