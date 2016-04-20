make.gene.matrix <-
function(metadata, locusCol = 'cleanedGeneRegion', vouchersCol = 'newLabels', ncbiCol = 'NCBI_accession', orgsCol = 'organism', logerrors = TRUE, verbose = FALSE) {
## take vouchers and regions to make a matrix we can use
## Arguments:
##  metadata = parsed data from NCBI genbank
##  loci = translation from verbatim gene regions (NCBI) to a cleaned up gene regions name; relevant columns are "verbatim" and "clean"; every genbank row should correspond with an entry in the verbatim column
##  spmCol = the name of the specimen label column, if it has been creaated; if not, voucherFormula is used to create it

  missingVouchers <- which(gsub(" ", "", metadata[[vouchersCol]], fixed = TRUE) == "")
  uniqueLoci <- unique(sort(as.character(metadata[[locusCol]])))
  uniqueVouchers <- unique(sort(as.character(metadata[[vouchersCol]])))
  out <- matrix('', nrow = length(uniqueVouchers), ncol = length(uniqueLoci), dimnames = list(uniqueVouchers, uniqueLoci))

  # vector of organisms
  orgs <- sapply(uniqueVouchers, function(x) paste(as.character(unique(metadata[[orgsCol]][metadata[[vouchersCol]] %in% x])), collapse = "|"))

  # vector of NCBI accessions
  ncbiAcc <- sapply(uniqueVouchers, function(x) paste(as.character(unique(metadata[[ncbiCol]][metadata[[vouchersCol]] %in% x])), collapse = "|"))

  # populate the matrix
  meta.orig <- metadata
  if(length(missingVouchers) > 0 ) metadata <- metadata[-missingVouchers,]
  # browser()
  for (i in 1:dim(metadata)[1]) {
    if(!any(is.na(metadata[i, c('cleanedGeneRegion', 'cleanedVoucher')]))) {
	  if(verbose) message(paste('doing', i))
	  out[metadata[i, vouchersCol], metadata[i, locusCol]] <- ifelse(out[metadata[i, vouchersCol], metadata[i, locusCol]] == '',
	                                                                 as.character(metadata[i, ncbiCol]),
																	 paste(out[metadata[i, vouchersCol], metadata[i, locusCol]], metadata[i, ncbiCol], sep = '|')
																					 )
	  }
	else(message(paste("Row", i, "of your metadata table seems to have a problem")))
	} # close i
  numberOfOrgs <- sapply(strsplit(as.character(orgs), "|", fixed = T), length)
  numberOfAccessions <- sapply(strsplit(as.character(ncbiAcc), "|", fixed = T), length)
  # numberOfSequences <- apply(out, 1, sum)
  out <- cbind(orgs, ncbiAcc, numberOfOrgs, numberOfAccessions, as.data.frame(out))

  if(logerrors) write.csv(metadata[missingVouchers, ], paste('missingVouchers.log.', paste(sample(letters,5), collapse = ''), '.csv', sep = ''))
  
  # class(out) <- c('geneMat', 'matrix')
  return(out)
  }
