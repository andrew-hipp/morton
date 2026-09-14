#' Asymmetric Mantel test
#'
#' Performs an asymmetric Mantel test between two distance objects.
#' Not checked yet!!!!
#'
#' @param x The first distance object.
#' @param y The second distance object.
#' @param which1 The set of individuals in set 1
#' @param which2 The set of individuals in set 2
#' @param nreps The number of replicate permutations. Defaults to 999.
#'
#' @return Currently returns `NULL`.
#' @export
mantel.asym <- function(x, y, which1 = x$Labels, which2 = x$Labels, nreps = 999) {
    x.mat <- as.matrix(x)
    y.mat <- as.matrix(y)
    x.orig <- x[which1, which2]
    y.orig <- y[which1, which2]
    r.obs <- cor(as.numeric(x.orig, y.orig))
    r.perm <- sapply(seq(length(nreps)), function(x) {
        x.perm = x
        x.perm$Labels = sample(x.perm$Labels)
        x.perm.mat <- as.matrix(x.perm)[which1, which2]
        out <- cor(as.numeric(x.perm.mat, y.orig))
        return(out)
    }
    p <- sum(|r.perm| > |r.obs|))
    out <- c(r.obs = r.obs, p = p)
    attributes(out, "perms") <- r.perm
    return(out)
}