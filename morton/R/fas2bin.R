fas2bin <-
function(infile, outfile, ...) {
  require(seqinr)
  in.bin <- make.binary(t(as.data.frame(read.fasta(infile, seqtype = "AA", set.attributes=F))))
  out.seqs <- vector('list', dim(in.bin)[1])
  out.names <- character(dim(in.bin)[1])
  for(i in 1:dim(in.bin)[1]) {
    out.seqs[[i]] <- in.bin[i, ]
	out.names[[i]] <- row.names(in.bin)[i]
    }
  write.fasta(out.seqs, out.names, outfile, ...)
  return(in.bin)
  }
