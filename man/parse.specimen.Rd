\name{parse.specimen}
\alias{parse.specimen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
parse.specimen(ncbiDat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ncbiDat}{
%%     ~~Describe \code{ncbiDat} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (ncbiDat) 
{
    fields <- c("All collectors", "Primary collector last name", 
        "Collector number", "Collection", "Unedited text")
    obj <- ncbiDat[, "specimen_voucher"]
    ac <- pcln <- cn <- coll <- character(length(obj))
    obj.split <- strsplit(obj, " ")
    ac <- gsub("s.n.", "", trim(sapply(strsplit(obj, "[0123456789]"), 
        function(x) x[1])), fixed = T)
    ac[grep(":", ac)] <- sapply(strsplit(ac[grep(":", ac)], ":"), 
        function(x) x[2])
    use.collected.by <- which(apply(cbind(ncbiDat[, "collected_by"], 
        ac), 1, function(x) nchar(x[1]) > nchar(x[2])))
    ac[use.collected.by] <- ncbiDat[use.collected.by, "collected_by"]
    use.authors <- which(is.na(ac))
    ac[use.authors] <- paste("AUTHOR", ncbiDat[use.authors, "authors"], 
        sep = ":")
    ac.splitted <- strsplit(ac, "[. ,]")
    for (i in which(!is.na(ac))) {
        ac.temp <- ac.splitted[[i]][nchar(ac.splitted[[i]]) > 
            1]
        pcln[i] <- ac.temp[1]
    }
    countInstances <- function(x, pattern = as.character(0:9), 
        proportion = TRUE) {
        x.split <- strsplit(x, NULL)[[1]]
        x.sum <- sum(x.split \%in\% pattern)
        if (proportion) 
            out <- x.sum/nchar(x)
        else out <- x.sum
        return(out)
    }
    for (i in 1:length(obj)) {
        if (obj[i] == "" | is.na(obj[i])) 
            next
        temp <- sapply(obj.split[[i]], countInstances)
        cn[i] <- obj.split[[i]][which(temp == max(temp))][1]
    }
    sn <- unique(c(grep("s.n.", as.character(obj), fixed = T), 
        grep("s. n.", as.character(obj), fixed = T)))
    cn[sn] <- "s.n."
    colonDelimits <- grep(":", obj)
    coll[-colonDelimits] <- sapply(obj.split[-colonDelimits], 
        function(x) {
            out <- gsub("(", "", gsub(")", "", grep("(", x, fixed = T, 
                value = T), fixed = T), fixed = T)
            if (length(out) == 0) 
                return("")
            else return(out[length(out)])
        })
    coll[colonDelimits] <- sapply(obj.split[colonDelimits], function(x) strsplit(x, 
        ":")[[1]][1])
    coll <- unlist(coll)
    out <- data.frame(ac = ac, pcln = pcln, cn = cn, coll = coll, 
        dat = obj)
    names(out) <- fields
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
