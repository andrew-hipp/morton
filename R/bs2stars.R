bs2symbols <- function(tr, bootThresh = c(low=69, med=79, high=94), 
						symbols = c(low='*', med='**', high='***'),
						commonSymbol = '*'
						) {
    bVec <- tr$edge[, 2]-Ntip(tr)
    bVec[bVec < 1] <- NA
    bVec <- tr$node.label[bVec] %>% as.numeric
    bVec[is.na(bVec)] <- 0
    bVec[bVec > bootThresh['high']] <- symbols['high']
    bVec[bVec > bootThresh['med']] <- symbols['med']
    bVec[bVec > bootThresh['low']] <- symbols['low']
    bVec[grep(commonSymbol, bVec, fixed = T, invert = T)] <- ''
    return(bVec)
}
