tips.expected <-
function(tr, tips, value = FALSE, ...) {
## returns the number of tips descended from the mrca of a vector of tips
## called "expected" because this is the number of tips expected if the tips provided were a clade
  require(phangorn)
  if(length(tips) == 1) return(1)
  tips.mrca <- getMRCA(tr, tips)
  tips.descendants <- Descendants(tr, tips.mrca, ...)[[1]]
  if (value) out <- tips.descendants
  else out <- length(tips.descendants)
  out
  }
