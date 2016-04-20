gene.matrix.stats <-
function(mat, outfile = paste('geneStats.', paste(sample(letters, 5), collapse = ''), '.txt', sep = '')) {
## describes the gene matrix
  loci <- dimnames(mat)[[2]][!dimnames(mat)[[2]] %in% c("orgs","ncbiAcc","numberOfOrgs","numberOfAccessions","numberOfSequences")]
  out <- file(outfile, open = 'a')
  writeLines(paste('Unique taxa:', length(unique(mat$orgs))), con = out)
  writeLines(paste('Unique taxa, only one organism:', length(unique(mat$orgs[mat$numberOfOrgs == 1]))), con = out)
  writeLines('Unique taxa for each gene:\n----------------------', con = out)
  temp <- sapply(loci, function(x) length(unique(mat$orgs[mat[[x]] >0])))
  writeLines(paste(loci[order(temp, decreasing = TRUE)], ": ", sort(temp, decreasing = TRUE), sep = ""), con = out)
  close(out)
  }
