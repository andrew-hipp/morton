section.coloring <-
function(tr, tipChar = '-', tip.cex = 0.1, tiplty = 0,
         pdfTitle = paste('trial.', paste(sample(letters,3), collapse = ''), '.pdf', sep = ''),
         dist.cats = rep(1, length(tr$tip.label)), whiteOut = 'NA', xy.multiplier = 1.5, 
         offset.proportion = 0.03, writeLabels = 0.1, ...) {
## trying to get concentric rings of coloring
  tr <- read.tree(text = write.tree(tr)) # orders labels in reading order
  vectorToColorBy <- label.elements(tr, "|", returnNum = 6, fixed = T)
  #tr$tip.label <- vectorToColorBy
  colors <- colors()[as.factor(vectorToColorBy)]
  colors[vectorToColorBy %in% whiteOut] <- 'white'
  offset.levels <- dist.cats[vectorToColorBy]
  offset.levels[is.na(offset.levels)] <- 0
  tr$tip.label = rep(tipChar, length(tr$tip.label))
  a = plot(tr, 'fan', tip.color = colors, align.tip.label = tiplty, plot = FALSE, ...)
  offset.levels <- offset.levels * offset.proportion * abs(diff(a$x.lim))
  if(!is.na(pdfTitle)) pdf(pdfTitle)
  a = plot(tr, 'fan', tip.color = colors, align.tip.label = tiplty,
           x.lim = a$x.lim * xy.multiplier, y.lim = a$y.lim * xy.multiplier,
           label.offset = offset.levels, ...)
  if(writeLabels > 0) {
    unique.sections <- unique(vectorToColorBy)

    }
  if(!is.na(pdfTitle)) dev.off()
  return(a)
}
