\name{morton-undocumented-neiD}

\alias{neiD}
\alias{GhatXY}


\title{undocumented morton functions - neiD}

\description{\code{morton-undocumented} functions are currently undocumented}

\details{
\pkg{morton} is a work in progress. 
If you are interested in a function that is not yet documented, please contact Andrew Hipp (ahipp@mortonarb.org).

## Unbiased estimate of D based on Nei 1978, Genetics 89, equation (6)
## Arguments:
##  genotypes = a data.frame with column 1 = "locus", column 2 = "allele", and columns 3:length(names(genotypes)) = individuals
##  n = a matrix with rows = loci, columns = individuals
## nomenclature from Nei:
##  p[i], q[i] = frequency of allele i in populations X and Y respectively
##  x[i], y[i] = corresponding sample allele frequences
##  G[x] = mean(sum(p[i]^2)) over all loci in the genome (for the population)
##  G[y] = mean(sum(q[i]^2)) ...
##  G[xy] = mean(sum(p[i]*q[i])) ...
##  J[x], J[y], J[xy] = corresponding sample means
##  D = -log(G[xy] / sqrt(G[x] * G[y])) -- this is the biased D
## to makes this unbiased -- Dhat -- use Ghat in the place of G, calculated as:
##  Ghat[x] = mean((2 * n[x] * J[x] - 1) / (2 * n[x] - 1)) over loci studied
##  Ghat[y] = mean((2 * n[y] * J[y] - 1) / (2 * n[y] - 1)) ...
##  Ghat[xy] = J[xy]
## thus, equation (6) is: Dhat = -log(Ghat[xy] / sqrt(Ghat[x] * Ghat[y]))
## Note that in equation (12), Nei gives an unbiased estimate of single locus genetic distance based on the minimum distance for the kth locus as:
##  d[k] = (2 * n[x] * sum(x[i]^2) - 1) / (2 * (2 * n[x] - 1)) + (2 * n[y] * sum(y[i]^2) - 1) / (2 * (2 * n[y] - 1)) - sum(x[i] * y[i])
##  Dhat-m = mean(d[k])
## Andrew Hipp (ahipp@mortonarb.org), May 2008
}

