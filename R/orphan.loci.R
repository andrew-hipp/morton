orphan.loci <-
function(x)  {
## takes output from make.shared.gene.matrix
  return(names(which(colSums(x) == 0)))
  }
