\name{make.fasta.files}
\alias{make.fasta.files}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make.fasta.files(geneMatrix = geneMat.2013.06.17, seqDat = cariceae.2013.02.28, genes = top12, outdir = paste("fasta.out.", paste(sample(letters, 5), collapse = ""), format(Sys.time(), "\%Y-\%m-\%d"), sep = ""), maxtax = 1, treat.multiples = c("discard", "takefirst"), batchfile = "muscle")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{geneMatrix}{
%%     ~~Describe \code{geneMatrix} here~~
}
  \item{seqDat}{
%%     ~~Describe \code{seqDat} here~~
}
  \item{genes}{
%%     ~~Describe \code{genes} here~~
}
  \item{outdir}{
%%     ~~Describe \code{outdir} here~~
}
  \item{maxtax}{
%%     ~~Describe \code{maxtax} here~~
}
  \item{treat.multiples}{
%%     ~~Describe \code{treat.multiples} here~~
}
  \item{batchfile}{
%%     ~~Describe \code{batchfile} here~~
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
function (geneMatrix = geneMat.2013.06.17, seqDat = cariceae.2013.02.28, 
    genes = top12, outdir = paste("fasta.out.", paste(sample(letters, 
        5), collapse = ""), format(Sys.time(), "\%Y-\%m-\%d"), sep = ""), 
    maxtax = 1, treat.multiples = c("discard", "takefirst"), 
    batchfile = "muscle") 
{
    if (!outdir \%in\% dir()) 
        dir.create(outdir)
    geneMatrix <- geneMatrix[geneMatrix$numberOfOrgs <= maxtax, 
        ]
    geneMatrix$seqLabels <- tidyNames(paste(geneMatrix$orgs, 
        row.names(geneMatrix)))
    for (i in genes) {
        out.log <- cbind(geneMatrix[geneMatrix[[i]] != "", c("orgs", 
            "seqLabels", i)], whatToDo = "", comments = "")
        multiples <- grep("|", out.log[, i], fixed = T)
        if (treat.multiples[1] == "discard") {
            if (length(multiples) != 0) 
                out.log <- out.log[-multiples, ]
        }
        if (treat.multiples[1] == "takefirst") 
            out.log[multiples, i] <- sapply(out.log[multiples, 
                i], function(x) strsplit(x, "|", fixed = T)[[1]][1])
        seqs <- seqDat$Full_sequence[match(out.log[, i], seqDat$NCBI_accession)]
        writeLines(paste(">", out.log$seqLabels, "\n", seqs, 
            sep = ""), paste(outdir, "/", i, format(Sys.time(), 
            ".\%Y-\%m-\%d.fas"), sep = ""))
        write.csv(out.log, paste(outdir, "/", i, ".logfile.", 
            format(Sys.time(), "\%Y-\%m-\%d.csv"), sep = ""))
    }
    if (batchfile == "muscle") {
        dir.create(paste(outdir, "/muscle", sep = ""))
        littleFiles <- dir(outdir, full = FALSE, patt = ".fas")
        writeLines(paste("muscle3.8.31_i86win32 -in ", paste("../", 
            littleFiles, sep = ""), " -out ", paste(littleFiles, 
            ".muscled.fas", sep = ""), sep = ""), paste(outdir, 
            "/muscle/muscle.bat", sep = ""))
    }
    return("done!")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
