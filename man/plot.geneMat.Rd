\name{plot.geneMat}
\alias{plot.geneMat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.geneMat(x, tr = NA, genes = colnames(x)[6:(dim(x)[2])], panes = c(5, 1, 2), margins = c(0, 1, 0.5, 0), geneColors = c("red", "black"), minGenes = 10, sortByFreq = T, label.cex = 0.7, pdfTitle = format(Sys.time(), "geneMat.plot.\%Y.\%m.\%d.\%H\%M.pdf"), pdfW = 10, pdfH = 10, remove.tips = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{tr}{
%%     ~~Describe \code{tr} here~~
}
  \item{genes}{
%%     ~~Describe \code{genes} here~~
}
  \item{panes}{
%%     ~~Describe \code{panes} here~~
}
  \item{margins}{
%%     ~~Describe \code{margins} here~~
}
  \item{geneColors}{
%%     ~~Describe \code{geneColors} here~~
}
  \item{minGenes}{
%%     ~~Describe \code{minGenes} here~~
}
  \item{sortByFreq}{
%%     ~~Describe \code{sortByFreq} here~~
}
  \item{label.cex}{
%%     ~~Describe \code{label.cex} here~~
}
  \item{pdfTitle}{
%%     ~~Describe \code{pdfTitle} here~~
}
  \item{pdfW}{
%%     ~~Describe \code{pdfW} here~~
}
  \item{pdfH}{
%%     ~~Describe \code{pdfH} here~~
}
  \item{remove.tips}{
%%     ~~Describe \code{remove.tips} here~~
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
function (x, tr = NA, genes = colnames(x)[6:(dim(x)[2])], panes = c(5, 
    1, 2), margins = c(0, 1, 0.5, 0), geneColors = c("red", "black"), 
    minGenes = 10, sortByFreq = T, label.cex = 0.7, pdfTitle = format(Sys.time(), 
        "geneMat.plot.\%Y.\%m.\%d.\%H\%M.pdf"), pdfW = 10, pdfH = 10, 
    remove.tips = TRUE, ...) 
{
    require(ape)
    if (!is.na(pdfTitle)) 
        pdf(pdfTitle, pdfW, pdfH)
    x <- t(x)
    x <- x[genes, ]
    if (sortByFreq) 
        x <- x[names(sort(rowSums(x != ""), decreasing = T)), 
            ]
    if (!is.na(tr[1])) {
        tr <- read.tree(text = write.tree(tr))
        if (any(!tr$tip.label \%in\% colnames(x))) {
            warning("not all tips in matrix; deleting unfound tips")
            tr <- drop.tip(tr, tr$tip.label[which(!tr$tip.label \%in\% 
                colnames(x))])
        }
        inds <- tr$tip.label
        x <- x[, inds]
        layout(matrix(1:3, 3, 1), heights = panes)
    }
    x <- x[apply(x, 1, function(y) sum(y != "")) >= minGenes, 
        ]
    nInds <- dim(x)[2]
    nGenes <- dim(x)[1]
    col.genes <- rep(geneColors, nGenes/length(geneColors))
    par(mai = margins)
    out.ngenes <- plot(1, xlim = c(1, nInds), ylim = c(1, nGenes), 
        type = "n", xlab = "", ylab = "", axes = F)
    axis(2, at = seq(from = 1, to = nGenes - 1, by = 2), labels = row.names(x)[seq(from = 1, 
        to = nGenes - 1, by = 2)], las = 2, cex.axis = label.cex, 
        col.axis = col.genes[1])
    axis(2, at = seq(from = 2, to = nGenes, by = 2), labels = row.names(x)[seq(from = 2, 
        to = nGenes, by = 2)], las = 2, cex.axis = label.cex, 
        col.axis = col.genes[2])
    for (i in seq(nGenes)) {
        for (j in seq(nInds)) {
            if (x[i, j] != "") 
                points(j, i, col = col.genes[i], ...)
        }
    }
    if (!is.na(tr[1])) {
        geneSums <- apply(x, 2, function(y) sum(y != ""))
        out.geneSums <- plot(geneSums, type = "p", pch = 19, 
            axes = F, ylab = "Genes sampled", cex.lab = 0.8)
        abline(h = mean(geneSums), lty = "dashed")
        axis(2, at = seq(geneSums), labels = seq(geneSums), las = 2)
        if (remove.tips) 
            tr$tip.label <- rep("", length(tr$tip.label))
        out.tree <- plot(tr, direction = "upwards", show.tip.label = F, 
            no.margin = F)
    }
    if (!is.na(pdfTitle)) 
        dev.off()
    out <- list(tree = out.tree, geneSums = out.geneSums, ngenes = out.ngenes)
    invisible(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
