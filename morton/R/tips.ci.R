tips.ci <-
function(tr, tipsBoolean, ...) {
## finds CI of any boolean tip vector
  require(phangorn)
  tipsBoolean <- as.integer(tipsBoolean)
  tips <- phyDat(matrix(tipsBoolean, length(tipsBoolean), 1, dimnames = list(tr$tip.label, NULL)), type = 'USER', levels = 0:1)
  out <- CI(tr, tips)
  out
  }
