\name{refine.fasta}
\alias{refine.fasta}
\title{
Clean Up FASTA Files to User Specification
}
\description{
Reverse-complement or delete sequences in a multiple sequence alignment based on a csv file input.
}
\usage{
refine.fasta(basedir = choose.dir())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{basedir}{
The address of the directory which contains the FASTA files to be refined and corresponding log files.
}
}
\details{
\code{\link{make.fasta.files}} automates the export of FASTA and log files. 
A separate FASTA file and corresponding log file is exported for each
locus included in the analysis.

Any log files used in this function must match the format of those exported 
by \code{\link{make.fasta.files}}.
Log files must be in csv format, and have the following columns:
  - orgs
  - seqLabels
  - whatToDo
  - comments
\code{orgs} is the taxon name of the specimen. \code{seqLabels} is the 
label of the sequence in the corresponding alignment. \code{whatToDo} 
has three possible states depending on what needs to be done to a 
sequence in the alignment. 
  - \code{RC} = reverse complement
  - \code{D} = delete
  - blank = do nothing
Log files start blank when exported. The user should manually edit 
log files to add these values before running \code{refine.fasta}.
}
\value{
Returns the string 'done!' when fasta files have been imported and
processed. Function invoked for side-effect.
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
%% \code{\link{make.gene.matrix}}, \code{\link{parse.INSDSeq}} ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (basedir = choose.dir()) {
    all.fasta <- sort(dir(basedir, full = T, patt = ".fas"))
    all.logs <- sort(dir(basedir, full = T, patt = "logfile"))
    files.out <- NULL
    for (i in 1:length(all.logs)) {
        toDo <- read.csv(all.logs[i], as.is = TRUE)
        if (!"whatToDo" \%in\% names(toDo)) {
            next
        }
        toDo <- toDo[toDo$whatToDo != "", ]
        if (nrow(toDo) == 0) {
            next
        }
        fasta <- read.dna(all.fasta[i], format = "fasta", as.character = TRUE)
        for (j in 1:nrow(toDo)) {
            if (toDo[j, "whatToDo"] == "RC") {
                fasta[[toDo$seqLabels[j]]] <- strsplit(as.character(reverseComplement(DNAString(paste(fasta[[toDo$seqLabels[j]]], 
                                                       collapse = "")))), "")
            }
            if (toDo[j, "whatToDo"] == "D") {
                fasta <- fasta[-(which(names(fasta) == toDo$seqLabels[j]))]
            }
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
\keyword{methods}
\keyword{file}
