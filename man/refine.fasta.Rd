\name{refine.fasta}
\alias{refine.fasta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
refine.fasta(basedir = choose.dir())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{basedir}{
%%     ~~Describe \code{basedir} here~~
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
function (basedir = choose.dir()) 
{
    require(Biostrings)
    require(ape)
    all.fasta <- sort(dir(basedir, full = T, patt = ".fas"))
    all.logs <- sort(dir(basedir, full = T, patt = "logfile"))
    files.out <- NULL
    for (i in 1:length(all.logs)) {
        toDo <- read.csv(all.logs[i], as.is = TRUE)
        if (!"whatToDo" \%in\% names(toDo)) 
            next
        toDo <- toDo[toDo$whatToDo != "", ]
        if (nrow(toDo) == 0) 
            next
        fasta <- read.dna(all.fasta[i], format = "fasta", as.character = TRUE)
        for (j in 1:nrow(toDo)) {
            if (toDo[j, "whatToDo"] == "RC") 
                fasta[[toDo$seqLabels[j]]] <- strsplit(as.character(reverseComplement(DNAString(paste(fasta[[toDo$seqLabels[j]]], 
                  collapse = "")))), "")
            if (toDo[j, "whatToDo"] == "D") 
                fasta <- fasta[-(which(names(fasta) == toDo$seqLabels[j]))]
        }
        file.out <- paste(all.fasta[i], ".cleaned.fas", sep = "")
        files.out <- c(files.out, file.out)
        write.dna(fasta, file.out, format = "fasta")
    }
    writeLines(paste("muscle3.8.31_i86win32 -in ", files.out, 
        " -out ", paste(files.out, ".muscled.fas", sep = ""), 
        sep = ""), paste(basedir, "/muscle/muscle.bat", sep = ""))
    return("done!")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
