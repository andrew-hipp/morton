\name{make.unique.vouchers}
\alias{make.unique.vouchers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make.unique.vouchers(metadata, voucherFormula = c("Primary.collector.last.name", "Collector.number", "isolate", "CollectionNumber", "Collection"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{metadata}{
%%     ~~Describe \code{metadata} here~~
}
  \item{voucherFormula}{
%%     ~~Describe \code{voucherFormula} here~~
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
function (metadata, voucherFormula = c("Primary.collector.last.name", 
    "Collector.number", "isolate", "CollectionNumber", "Collection")) 
{
    vouchers.ln.cn <- apply(metadata[voucherFormula], 1, function(x) paste(tidyName(x[!is.na(x)]), 
        collapse = ""))
    names(vouchers.ln.cn) <- metadata$NCBI_voucher
    return(vouchers.ln.cn)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
