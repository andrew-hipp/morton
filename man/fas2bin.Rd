\name{fas2bin}
\alias{fas2bin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fas2bin(infile, outfile, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{infile}{
%%     ~~Describe \code{infile} here~~
}
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (infile, outfile, ...) 
{
    in.bin <- make.binary(t(as.data.frame(read.fasta(infile, 
        seqtype = "AA", set.attributes = F))))
    out.seqs <- vector("list", dim(in.bin)[1])
    out.names <- character(dim(in.bin)[1])
    for (i in 1:dim(in.bin)[1]) {
        out.seqs[[i]] <- in.bin[i, ]
        out.names[[i]] <- row.names(in.bin)[i]
    }
    write.fasta(out.seqs, out.names, outfile, ...)
    return(in.bin)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
