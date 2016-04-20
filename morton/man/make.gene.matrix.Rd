\name{make.gene.matrix}
\alias{make.gene.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make.gene.matrix(metadata, locusCol = "cleanedGeneRegion", vouchersCol = "newLabels", ncbiCol = "NCBI_accession", orgsCol = "organism", logerrors = TRUE, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{metadata}{
%%     ~~Describe \code{metadata} here~~
}
  \item{locusCol}{
%%     ~~Describe \code{locusCol} here~~
}
  \item{vouchersCol}{
%%     ~~Describe \code{vouchersCol} here~~
}
  \item{ncbiCol}{
%%     ~~Describe \code{ncbiCol} here~~
}
  \item{orgsCol}{
%%     ~~Describe \code{orgsCol} here~~
}
  \item{logerrors}{
%%     ~~Describe \code{logerrors} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (metadata, locusCol = "cleanedGeneRegion", vouchersCol = "newLabels", 
    ncbiCol = "NCBI_accession", orgsCol = "organism", logerrors = TRUE, 
    verbose = FALSE) 
{
    missingVouchers <- which(gsub(" ", "", metadata[[vouchersCol]], 
        fixed = TRUE) == "")
    uniqueLoci <- unique(sort(as.character(metadata[[locusCol]])))
    uniqueVouchers <- unique(sort(as.character(metadata[[vouchersCol]])))
    out <- matrix("", nrow = length(uniqueVouchers), ncol = length(uniqueLoci), 
        dimnames = list(uniqueVouchers, uniqueLoci))
    orgs <- sapply(uniqueVouchers, function(x) paste(as.character(unique(metadata[[orgsCol]][metadata[[vouchersCol]] \%in\% 
        x])), collapse = "|"))
    ncbiAcc <- sapply(uniqueVouchers, function(x) paste(as.character(unique(metadata[[ncbiCol]][metadata[[vouchersCol]] \%in\% 
        x])), collapse = "|"))
    meta.orig <- metadata
    if (length(missingVouchers) > 0) 
        metadata <- metadata[-missingVouchers, ]
    for (i in 1:dim(metadata)[1]) {
        if (!any(is.na(metadata[i, c("cleanedGeneRegion", "cleanedVoucher")]))) {
            if (verbose) 
                message(paste("doing", i))
            out[metadata[i, vouchersCol], metadata[i, locusCol]] <- ifelse(out[metadata[i, 
                vouchersCol], metadata[i, locusCol]] == "", as.character(metadata[i, 
                ncbiCol]), paste(out[metadata[i, vouchersCol], 
                metadata[i, locusCol]], metadata[i, ncbiCol], 
                sep = "|"))
        }
        else (message(paste("Row", i, "of your metadata table seems to have a problem")))
    }
    numberOfOrgs <- sapply(strsplit(as.character(orgs), "|", 
        fixed = T), length)
    numberOfAccessions <- sapply(strsplit(as.character(ncbiAcc), 
        "|", fixed = T), length)
    out <- cbind(orgs, ncbiAcc, numberOfOrgs, numberOfAccessions, 
        as.data.frame(out))
    if (logerrors) 
        write.csv(metadata[missingVouchers, ], paste("missingVouchers.log.", 
            paste(sample(letters, 5), collapse = ""), ".csv", 
            sep = ""))
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
