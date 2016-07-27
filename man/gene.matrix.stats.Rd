\name{summary.geneMat}
\alias{summary.geneMat}
\title{
\code{Summary} method for gene.matrix.
}
\description{
Write a text file to the working directory which details the summary statistics a multiple locus sequence dataset following processing by \code{make.gene.matrix}.
}
\usage{
gene.matrix.stats(mat, outfile = paste("geneStats.",
                                       paste(sample(letters, 5), collapse = ""), 
                                       ".txt", 
                                       sep = ""))
}
\arguments{
  \item{mat}{
A matrix, the output of \code{make.gene.matrix}, connecting gene sequences to specimen voucher.
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

\keyword{file}
\keyword{methods}