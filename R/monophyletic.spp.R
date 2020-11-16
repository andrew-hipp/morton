monophyletic.spp <-
function(tree, ...) {
  require(ape)
  all.spp <- label.elements(tree, ...)
  unique.spp <- sort(unique(all.spp))
  out <- list(numSpp = length(unique.spp),
              spp.summary = cbind(count = sapply(unique.spp, function(x) sum(all.spp == x)),
			                      monophyletic = sapply(unique.spp, function(x) is.monophyletic(tree, names(all.spp)[all.spp == x])),
                            ci = sapply(unique.spp, function(x) tips.ci(tree, all.spp == x))
								  ))
  return(out)
  }
