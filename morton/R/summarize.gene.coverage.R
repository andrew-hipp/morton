summarize.gene.coverage <-
function(datMat = cariceae.concat.4genes.summary, extra.left = 2, ...) {
  genes.no = dim(datMat)[2]
  inds.no = dim(datMat)[1]
  par(mar = c(5, 4 + extra.left, 4, 2) + 0.1)
  plot(0, type = "n", xlim = c(1,inds.no), ylim = c(0, genes.no+1), xlab = 'Individual sequenced [number]', ylab = '', axes = FALSE)
  axis(2, at = seq(1:genes.no), labels = names(datMat), tick = FALSE, las = 1)
  axis(1)
  for(i in 1:genes.no) points(which(datMat[1:inds.no, i] == 1), rep(i, sum(datMat[1:inds.no, i])), col = i, ...)
  return('done')
  }
