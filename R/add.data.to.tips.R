add.data.to.tips <-
function(tr, datMat, delim = '[_|]',
returnNum = 1:2, returnDelim = "_", addCols = c('GROUP'),
uniques = F, addDelim = "|", reorderTree = TRUE, ...) {
## adds label to the end of the tips using a standard formula
## returns the relabelled tree and a matrix indicating what tip got what section
## arguments:
##   tr = tree
##   datMat = matrix with the data you want added as columns, row.names matching tips of the tree after scrubbing through label.elements
  if(reorderTree) tr <- read.tree(text = write.tree(tr))
  tips.to.match <- label.elements(tr, delim, returnNum, returnDelim, ...)
  if(uniques) tips.to.drop <- which(duplicated(tips.to.match))
  addVect <- datMat[match(tips.to.match, row.names(datMat)), addCols]
  if(!is.null(dim(addVect))) addVect <- apply(addVect, 1, paste, collapse = addDelim)
  oldNames <- tr$tip.label
  newLabel <- paste(oldNames, addVect, sep = addDelim)
  tr$tip.label <- newLabel
  if(uniques) tr <- drop.tip(tr, tr$tip.label[tips.to.drop])
  out = list(tr.relabelled = tr, labelMat = cbind(oldLabel = oldNames, tipMatched = tips.to.match, newElement = addVect, newLabel = newLabel, retained = !duplicated(tips.to.match)))
  row.names(out$labelMat) <- out$labelMat[, 'newLabel']
  return(out)
}
