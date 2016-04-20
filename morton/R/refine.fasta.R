refine.fasta <-
function(basedir = choose.dir()) {
## goes through the log files in a directory and makes changes according to these codes:
##  D = delete
##  RC = reverse and complement
  require(Biostrings)
  require(ape)
  all.fasta <- sort(dir(basedir, full = T, patt = '.fas'))
  all.logs <- sort(dir(basedir, full = T, patt = 'logfile'))
  files.out <- NULL
  for(i in 1:length(all.logs)) {
    toDo <- read.csv(all.logs[i], as.is = TRUE)
	if(!"whatToDo" %in% names(toDo)) next
	toDo <- toDo[toDo$whatToDo != '', ]
	if(nrow(toDo) == 0) next
	fasta <- read.dna(all.fasta[i], format = 'fasta', as.character = TRUE)
	for(j in 1:nrow(toDo)) {
	  if(toDo[j, 'whatToDo'] == 'RC') fasta[[toDo$seqLabels[j]]] <- strsplit(as.character(reverseComplement(DNAString(paste(fasta[[toDo$seqLabels[j]]], collapse = '')))), '')
	  if(toDo[j, 'whatToDo'] == 'D') fasta <- fasta[-(which(names(fasta) == toDo$seqLabels[j]))]
	  }
	file.out <- paste(all.fasta[i],'.cleaned.fas', sep = '')
	files.out <- c(files.out, file.out)
	write.dna(fasta, file.out, format = 'fasta')
	}
	writeLines(paste('muscle3.8.31_i86win32 -in ', files.out, ' -out ', paste(files.out, '.muscled.fas', sep = ''), sep = ''), paste(basedir, '/muscle/muscle.bat', sep = ''))
  return('done!')
  }
