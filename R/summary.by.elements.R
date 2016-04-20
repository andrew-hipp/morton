summary.by.elements <-
function(tr, returnEffectOnTreeLength = TRUE, minSizeForEffect = 3, recommendDrops = TRUE, ...) {
## this is the taxonomic disparity function
  all.elements <- label.elements(tr, ...)
  unique.elements <- sort(unique(all.elements))
  out <- cbind(count = sapply(unique.elements, function(x) sum(all.elements == x)),
               expected = sapply(unique.elements, function(x) tips.expected(tr, names(all.elements)[all.elements == x]))
			   )
  out <- list(disparity.mat = cbind(out, disparity = out[, 'expected'] - out[, 'count']), effectSize = NA)
  if(returnEffectOnTreeLength) {
    message('calculating effect size... please be patient')
	effectOnTreeLength <- lapply(unique.elements, function(x) {
	  if(out$disparity.mat[x, 'count'] < minSizeForEffect) return(0)
	  message(paste('doing effect size for', x))
	  tr.temp <- drop.tip(tr, tr$tip.label[which(all.elements != x)])
	  effectSizeByTaxon <- sum(tr.temp$edge.length) - sapply(tr.temp$tip.label, function(y) sum(drop.tip(tr.temp, y)$edge.length))
	  return(effectSizeByTaxon)
	  })
	out$effectSize <- effectOnTreeLength
	names(out$effectSize) <- unique.elements
	}
  out
  }
