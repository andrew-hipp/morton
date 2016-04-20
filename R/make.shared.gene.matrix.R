make.shared.gene.matrix <-
function(dat, loci = dimnames(dat)[[2]][!dimnames(dat)[[2]] %in% c("orgs","ncbiAcc","numberOfOrgs","numberOfAccessions","numberOfSequences")]) {
## takes output from make.gene.matrix
  out <- matrix(NA, length(loci), length(loci), dimnames = list(loci, loci))
  for(i in 1:length(loci)) {
    for(j in 1:length(loci)) {
	  out[loci[i], loci[j]] <- sum(ifelse(dat[[loci[i]]] == 0, 0, 1) + ifelse(dat[[loci[j]]] == 0, 0, 1) == 2)
	  } # close j
	} # close i
  return(out)
  }
