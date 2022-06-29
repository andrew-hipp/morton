# make a little phylo
library(ape)
library(phytools)
library(magrittr)

simplePhylo <- function(tips = NULL, tr = NULL, nodes = NULL,
                          weldTree = NULL, weldTreeGrep = NULL) {
  ## Arguments
    # tips: a vector of tip names
    # tr: the base phylogeny to use
    # nodes: a data.frame with columns 'tip', 'node', 'distUp'
      # tip : tip to add
      # node : pipe-delimited taxon list that tells where to add what node
      # distUp : distance in branch-length units to place each tip toward root of tree
    # weldTree : subtree to add on if needed
    # weldTreeGrep : pipe-delimited taxon list telling where to add weldTree

  ## grab names if nothing is specified
  if(is.null(tips)) {
    tipNames <-
      readLines('https://raw.githubusercontent.com/andrew-hipp/morton/master/data/mor.exp.2022.txt') %>%
      gsub(pattern = ' ', replacement = '_')
    } else tipNames <- tips

  ## get Smith and Brown 2018 tree if needed
  if(is.null(tr)) {
    if(!exists('dat.smithBrown')) {
      smithBrownTree <- 'https://github.com/FePhyFoFum/big_seed_plant_trees/releases/download/v0.1/v0.1.zip'
      temp <- tempfile()
      download.file(smithBrownTree,temp)
      dat.smithBrown <- read.tree(unz(temp, "v0.1/ALLMB.tre"))
      unlink(temp)
    } # close if ! exists
    tr <- dat.smithBrown
  } # close if is.null tr

  ## weld on singletons
  if(!is.null(nodes)) {
    message('binding new tips; still troublshooting')
    for(i in seq(dim(nodes)[1])) {
      message(paste('... binding', nodes$tip[i], 'to phylogeny'))
      tipGrep <- grep(nodes$node[i], tr$tip.label, value = T)
      nodeTemp <- findMRCA(tr, tipGrep)
      if(is.null(nodeTemp)) {
        message('   ... sister to a tip...')
        nodeTemp <- grep(tipGrep, tr$tip.label)
      }
    if(tr$edge.length[which(tr$edge[, 2] == 65)] < nodes$distUp[i]) {
      message(paste('   ... using smaller value for distUp; branch too short...'))
      nodes$distUp[i] <- 0.5 * tr$edge.length[which(tr$edge[, 2] == 65)]
    } # closing correction for short branches
    tr <- bind.tip(tr, nodes$tip[i],
                    where = nodeTemp, position = nodes$distUp[i])
    rm(nodeTemp)
  } # close for i
} # close if !is.null nodes

  ## weld on subtree
  if(!is.null(weldTree)) {
    message('doing this stuff -- still to be coded')
  tr <-
    drop.tip(tr,
              grep(weldTreeGrep, tr$tip.label, value = T),
              trim.internal = F
            )

  dat.subtreeDepth <-
    max(node.depth.edgelength(tr)) -
    node.depth.edgelength(tr)[which(tr$tip.label == '')]

  subtreePos <-
    max(weldTree %>% node.depth.edgelength) - dat.subtreeDepth

  tr <-
    bind.tree(tr, weldTree,
              where = which(tr$tip.label == ''),
              position = subtreePos)
  tr <- drop.tip(tr, '')
  } # close weldTree subroutine

  ## prune to taxa
  tr <- drop.tip(tr, setdiff(tr$tip.label, tipNames))
  if(any(!tipNames %in% tr$tip.label)) {
    message('*** You are missing a few names in the tree ***')
    print(setdiff(tipNames, tr$tip.label))
  } # close if
  tr <- force.ultrametric(tr)

  ## return trees
  return(tr)

} # close function
