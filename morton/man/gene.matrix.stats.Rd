\name{gene.matrix.stats}
\alias{gene.matrix.stats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gene.matrix.stats(mat = geneMat.2013.06.13, outfile = paste("geneStats.", paste(sample(letters, 5), collapse = ""), ".txt", sep = ""))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
%%     ~~Describe \code{mat} here~~
}
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
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
function (mat = geneMat.2013.06.13, outfile = paste("geneStats.", 
    paste(sample(letters, 5), collapse = ""), ".txt", sep = "")) 
{
    loci <- dimnames(mat)[[2]][!dimnames(mat)[[2]] \%in\% c("orgs", 
        "ncbiAcc", "numberOfOrgs", "numberOfAccessions", "numberOfSequences")]
    out <- file(outfile, open = "a")
    writeLines(paste("Unique taxa:", length(unique(mat$orgs))), 
        con = out)
    writeLines(paste("Unique taxa, only one organism:", length(unique(mat$orgs[mat$numberOfOrgs == 
        1]))), con = out)
    writeLines("Unique taxa for each gene:\n----------------------", 
        con = out)
    temp <- sapply(loci, function(x) length(unique(mat$orgs[mat[[x]] > 
        0])))
    writeLines(paste(loci[order(temp, decreasing = TRUE)], ": ", 
        sort(temp, decreasing = TRUE), sep = ""), con = out)
    close(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
