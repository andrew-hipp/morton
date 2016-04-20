color.tips.by.element <-
function(tr, element = 6, delim = "|", fixed = TRUE, whiteOut = 'NA', addLegend = T, colorIt = FALSE, byLabels = TRUE, tip.cex = 0.1, dot.pch = 1, blank.tips = T, ...) {
  vectorToColorBy <- label.elements(tr, delim, returnNum = element, fixed = fixed)
  #tr$tip.label <- vectorToColorBy
  colors = colors()[as.factor(vectorToColorBy)]
  colors[vectorToColorBy %in% whiteOut] <- 'white'
  par(mar = c(5,10,4,2))
  if(blank.tips) tr$tip.label <- sapply(tr$tip.label, function(x) "")
  a = plot(tr, ...)
  if(colorIt) tiplabels(col = colors, pch = dot.pch, cex = tip.cex)
  if(byLabels) tiplabels(vectorToColorBy, cex = tip.cex, align.tip.label = T)
  if(addLegend) legend(a$x.lim[1] - abs(diff(a$x.lim) / 4), a$y.lim[2], legend = unique(vectorToColorBy), pch = dot.pch, cex = 1, col = unique(colors), bty = 'n')
  }
