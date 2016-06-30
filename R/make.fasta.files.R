make.fasta.files <-
function(geneMatrix, seqDat, genes, outdir = paste('fasta.out.', paste(sample(letters,5), collapse = ''), format(Sys.time(), "%Y-%m-%d"), sep = ''), maxtax = 1, treat.multiples = c('discard', 'takefirst'), batchfile = 'muscle') {
  # Purpose: make fasta files for individuals having the genes indicated
    # creates a log file with all individuals used for each gene
  # Arguments:
    #  geneMatrix: output from make.gene.matrix
    #  seqDat: sequence data from NCBI
    #  genes: a vector of genes to use, top12 is the output of the top.12 function.
    #  outdir: directory to write to
    #  maxtax: maximum number of taxon names allowable per individual
    #  treat.multiples: what to do for individuals that have multiple accessions 
      # for a given gene; currently either discards ('discard') them or takes 
      # the first NCBI accession ('takefirst')
  # Returns: the string 'done!'

  if(!outdir %in% dir()) dir.create(outdir)
  geneMatrix <- geneMatrix[geneMatrix$numberOfOrgs <= maxtax, ]
  geneMatrix$seqLabels <- tidyNames(paste(geneMatrix$orgs, row.names(geneMatrix)))
  # make individual gene files
  for(i in genes) {
	# if(i == 'rbcL') browser()
	out.log <- cbind(geneMatrix[geneMatrix[[i]] != '', c('orgs', 'seqLabels', i)], whatToDo = '', comments = '')
	multiples <- grep('|', out.log[, i], fixed = T)
	if(treat.multiples[1] == 'discard') {if(length(multiples) != 0) out.log <- out.log[-multiples, ]}
	if(treat.multiples[1] == 'takefirst') out.log[multiples, i] <- sapply(out.log[multiples, i], function(x) strsplit(x, "|", fixed = T)[[1]][1])
	seqs <- seqDat$Full_sequence[match(out.log[, i], seqDat$NCBI_accession)]
	writeLines(paste(">", out.log$seqLabels, "\n", seqs, sep = ''), paste(outdir, '/', i, format(Sys.time(), ".%Y-%m-%d.fas"), sep = ''))
	write.csv(out.log, paste(outdir, '/', i, '.logfile.', format(Sys.time(), "%Y-%m-%d.csv"), sep = ''))
  }

  ## make batch file
  if(batchfile == 'muscle') {
    dir.create(paste(outdir, '/muscle', sep = ''))
	# files <- dir(outdir, full = TRUE, patt = '.fas')
	littleFiles <- dir(outdir, full = FALSE, patt = '.fas')
	writeLines(paste('muscle3.8.31_i86win32 -in ', paste('../', littleFiles, sep = ''), ' -out ', paste(littleFiles, '.muscled.fas', sep = ''), sep = ''), paste(outdir, '/muscle/muscle.bat', sep = ''))
	}
  return('done!')
  }
