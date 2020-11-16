orphan.individuals <-
function(dat) {
## takes output from make.gene.matrix
  shared.loci <- make.shared.gene.matrix(dat)
  not.orphan.loci <- names(which(colSums(shared.loci) > 0))
  not.orphan.individuals <- apply(dat[, not.orphan.loci], 1, function(x) sum(x) > 0)
  orphan.individuals <- apply(dat[, not.orphan.loci], 1, function(x) sum(x) == 0)
  out <- list(NCBI_accession = dat$ncbiAcc, taxon = dat$orgs, orphans = orphan.individuals, notOrphans = not.orphan.individuals)
  return(out)
  }
