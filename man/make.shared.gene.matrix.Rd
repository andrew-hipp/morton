\name{make.shared.gene.matrix}
\alias{make.shared.gene.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make.shared.gene.matrix(dat = geneMat.2013.06.13, loci = dimnames(dat)[[2]][!dimnames(dat)[[2]] \%in\% c("orgs", "ncbiAcc", "numberOfOrgs", "numberOfAccessions", "numberOfSequences")])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{loci}{
%%     ~~Describe \code{loci} here~~
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
function (dat = geneMat.2013.06.13, loci = dimnames(dat)[[2]][!dimnames(dat)[[2]] \%in\% 
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
