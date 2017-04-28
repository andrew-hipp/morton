\name{plot.geneMat}
\alias{plot.geneMat}
\title{
Plot Gene Coverage of All Individuals
}
\description{
Plot gene coverage for each tip of a gene matrix produced by \code{make.gene.matrix},
where columns are tips and rows are genes.
}
\usage{
plot.geneMat(x, tr = NA, genes = colnames(x)[6:(dim(x)[2])], panes = c(5, 1, 2), margins = c(0, 1, 0.5, 0), geneColors = c("red", "black"), minGenes = 10, sortByFreq = T, label.cex = 0.7, pdfTitle = format(Sys.time(), "geneMat.plot.\%Y.\%m.\%d.\%H\%M.pdf"), pdfW = 10, pdfH = 10, remove.tips = TRUE, ...)
}
\arguments{
  \item{x}{
  The gene matrix from \code{make.gene.matrix}
}
  \item{tr}{
  Optional. A phylogenetic tree which dictates tip order in the columns. 
  The tree's topography is plotted above the plot corresponding to columns.
}
  \item{genes}{
  Optional. A vector of genes to include in the plot.
}
  \item{panes}{
  Optional. A vector with the heights of each portion of the plot
  to be passed to \code{layout}.
}
  \item{margins}{
  Optional. A vector with margin dimensions for the plot. See the 
  \code{par} argument \code{mai}.
}
  \item{geneColors}{
  Optional. A vector of the colors to use to mark each gene. If less
  than the number of genes are provided, then colors will repeat.
}
  \item{minGenes}{
  Optional. The minimum number of individuals which must have a
  gene sequence for that gene to be included in the plot.
}
  \item{sortByFreq}{
  Optional. A boolean determining whether genes should be ordered
  by the number of individuals with data for that gene, from
  most to least.
}
  \item{label.cex}{
  Optional. The \code{cex} value for row and column labels.
}
  \item{pdfTitle}{
  Optional. The name of the pdf file to be exported.
}
  \item{pdfW}{
  Optional. The width of the pdf file in inches.
}
  \item{pdfH}{
  Optional. The height of the pdf file in inches.
}
  \item{remove.tips}{
  Optional. A boolean determining whether or not tip labels should be plotted.
}
  \item{\dots}{
  Other arguments to be passed to the \code{points} function.
}
}
\details{
The default for \code{genes} is all columns in the gene matrix excluding the first five,
which contain summary statistics. The default for \code{panes} is \code{5,1,2}, and for
margins it is \code{0,1,0.5,0}. The default for \code{geneColors} is red and black.
The default for \code{minGenes} is 10, for \code{sortByFreq} it is \code{TRUE}, and for
\code{label.cex} it is 0.7. The default pdf file name is \code{geneMat.plot} plus the
date and time, delimited by periods. The default for PDF width and height are both 10.
Tip labels are removed from the plot by default.
}
\value{
  \item{tree }{The tree plotted above tips. If none, this value is NA.}
  \item{geneSums }{A plot of the number of individuals possessing data for each gene.}
  \item{ngenes}{A plot of the number of genes per individual.}
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
\code{\link{make.gene.matrix}}, \code{\link{layout}}, \code{\link{par}}
}

\keyword{ hplot }
\keyword{ file }