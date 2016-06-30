\name{summary.geneMat}
\alias{summary.geneMat}
\title{
\code{Summary} method for gene.matrix.
}
\description{
Write a text file to the working directory which details the summary statistics a multiple locus sequence dataset following processing by \code{\link{make.gene.matrix}}.
}
\usage{
gene.matrix.stats(mat, outfile = paste("geneStats.",
                                       paste(sample(letters, 5), collapse = ""), 
                                       ".txt", 
                                       sep = ""))
}
\arguments{
  \item{mat}{
A matrix, the output of \code{\link{make.gene.matrix}}, connecting gene sequences to specimen voucher.
}
  \item{outfile}{
An optional string, the name of the output text file. By default, it is geneStats followed by five random letters.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the string 'done!'.  Called for side-effect of writing summary file.
}
\author{
Kasey Pham and Andrew Hipp
}

\seealso{
\code{\link{make.gene.matrix}}
}
\examples{
## The function is currently defined as
function (mat, outfile = paste("geneStats.", paste(sample(letters,
    5), collapse = ""), ".txt", sep = ""))
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

\keyword{ file }
\keyword{methods}