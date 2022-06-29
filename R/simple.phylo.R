# make a little phylo
library(ape)
library(phytools)
library(magrittr)

simple.phylo <- function(tips = NULL, tr = NULL, nodes = NULL,
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
  tipNames <-
    readLines('https://raw.githubusercontent.com/andrew-hipp/morton/master/data/mor.exp.2022.txt') %>%
    gsub(pattern = ' ', replacement = '_')

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

  ## prune to taxa
  if(any(!tipNames %in% tr$tip.label)) {
    message('*** You are missing a few names in the tree ***')
    print(setdiff(tipNames, tr$tip.label))
  } # close if
  tr2 <- drop.tip(tr, setdiff(tr$tip.label, tipNames))
  tr2 <- force.ultrametric(tr2)

  ## weld on singletons
  if(!is.null(nodes)) {
    message('doing this stuff -- still to be coded')
  }

  ## weld on subtree
  if(!is.null(weldTree)) {
    message('doing this stuff -- still to be coded')
  }
  tr2 <-
    drop.tip(tr2,
              grep(weldTreeGrep, tr2$tip.label, value = T),
              trim.internal = F
            )

  dat.subtreeDepth <-
    max(node.depth.edgelength(tr2)) -
    node.depth.edgelength(tr2)[which(tr2$tip.label == '')]

  subtreePos <-
    max(weldTree %>% node.depth.edgelength) - dat.subtreeDepth

  tr2 <-
    bind.tree(tr2, weldTree,
              where = which(tr2$tip.label == ''),
              position = subtreePos)
  tr2 <- drop.tip(tr2, '')

  ## return trees
  return(tr2)
} # close function
