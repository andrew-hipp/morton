orphan.loci <-
function(x)  {
## takes output from make.shared.gene.matrix
  names(which(colSums(x) == 0))
  }
