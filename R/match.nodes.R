match.nodes <-
function(guideTree, otherTrees, matchBoots = T, plotBoots = T, ...) {
  if('phylo' %in% class(otherTrees)) otherTrees = list(tr1 = otherTrees)
  matched <- lapply(otherTrees, function(x) matchNodes(guideTree, x))
  match.mat <- do.call(cbind, c(guideTree = list(matched[[1]][, 1]), lapply(matched, function(x) x[, 2])))
  match.index <- which(apply(match.mat, 1, function(x) !any(is.na(x))))

  ## match up bootstraps
  if(matchBoots) {
    allTreesLabels <- lapply(c(guideTree = list(guideTree), otherTrees), function(x) c(x$tip.label, x$node.label))
    mat.boots <- match.mat[match.index, ]
    for(i in seq(dim(mat.boots)[2])) mat.boots[, i] <- as.integer(allTreesLabels[[i]][mat.boots[, i]])
    out <- list(mat.boots = mat.boots, mat.full = match.mat, mat.index = match.index)
    if(plotBoots) matplot(mat.boots[order(its.ets.matched$mat.boots[, 1], decreasing = T), ], ...)
    }
  else out <- list(mat.full = match.mat, mat.index = match.index)
  return(out)
  }
