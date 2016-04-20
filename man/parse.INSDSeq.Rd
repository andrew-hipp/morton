\name{parse.INSDSeq}
\alias{parse.INSDSeq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
parse.INSDSeq(xml_file, do = NA, includeSeqs = F, cores = 1, parse.specimens = T, qualsToUse = c("specimen_voucher", "country", "collection_date", "lat_lon", "note", "collected_by", "isolate", "pop_variant"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xml_file}{
%%     ~~Describe \code{xml_file} here~~
}
  \item{do}{
%%     ~~Describe \code{do} here~~
}
  \item{includeSeqs}{
%%     ~~Describe \code{includeSeqs} here~~
}
  \item{cores}{
%%     ~~Describe \code{cores} here~~
}
  \item{parse.specimens}{
%%     ~~Describe \code{parse.specimens} here~~
}
  \item{qualsToUse}{
%%     ~~Describe \code{qualsToUse} here~~
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
function (xml_file, do = NA, includeSeqs = F, cores = 1, parse.specimens = T, 
    qualsToUse = c("specimen_voucher", "country", "collection_date", 
        "lat_lon", "note", "collected_by", "isolate", "pop_variant")) 
{
    if (cores > 1 & Sys.info()["sysname"] == "Windows") 
        warning("Multicore is only supported on mac and linux for right now")
    nRecords <- length(xml_file$doc$children$INSDSet)
    columns <- c("NCBI_accession", "seq_length", "strandedness", 
        "moltype", "topology", "division", "update_date", "create_date", 
        "definition", "primary_accession", "accession_version", 
        " otherseq_IDS", "seq_source", "organism", "taxonomy", 
        "references", "feature_table", "qualifiers1", "generegion", 
        "Full_sequence", "authors")
    get.a.row <- function(recordNumber) {
        dat <- xml_file$doc$children$INSDSet[[recordNumber]]
        featuresL <- length(dat[["INSDSeq_feature-table"]][[1]][["INSDFeature_quals"]])
        featuresOut <- character(featuresL)
        for (i in seq(featuresL)) {
            if (length(dat[["INSDSeq_feature-table"]][[1]][["INSDFeature_quals"]][[i]]) != 
                2) 
                next
            featuresOut[i] <- xmlValue(dat[["INSDSeq_feature-table"]][[1]][["INSDFeature_quals"]][[i]][[2]])
            names(featuresOut)[i] <- xmlValue(dat[["INSDSeq_feature-table"]][[1]][["INSDFeature_quals"]][[i]][[1]])
        }
        readableFeatures = paste(names(featuresOut), featuresOut, 
            collapse = "|", sep = "_:_")
        featuresOutV <- featuresOut[qualsToUse]
        names(featuresOutV) <- qualsToUse
        out <- c(NCBI_accession = try(xmlValue(dat[["INSDSeq_locus"]]), 
            silent = T), seq_length = try(xmlValue(dat[["INSDSeq_length"]]), 
            silent = T), strandedness = try(xmlValue(dat[["INSDSeq_strandedness"]]), 
            silent = T), moltype = try(xmlValue(dat[["INSDSeq_moltype"]]), 
            silent = T), topology = try(xmlValue(dat[["INSDSeq_topology"]]), 
            silent = T), division = try(xmlValue(dat[["INSDSeq_division"]]), 
            silent = T), update_date = try(xmlValue(dat[["INSDSeq_update-date"]]), 
            silent = T), create_date = try(xmlValue(dat[["INSDSeq_create-date"]]), 
            silent = T), definition = try(xmlValue(dat[["INSDSeq_definition"]]), 
            silent = T), primary_accession = try(xmlValue(dat[["INSDSeq_primary-accession"]]), 
            silent = T), accession_version = try(xmlValue(dat[["INSDSeq_accession-version"]]), 
            silent = T), otherseq_IDS = try(xmlValue(dat[["INSDSeq_other-seqids"]]), 
            silent = T), seq_source = try(xmlValue(dat[["INSDSeq_source"]]), 
            silent = T), organism = try(xmlValue(dat[["INSDSeq_organism"]]), 
            silent = T), taxonomy = try(xmlValue(dat[["INSDSeq_taxonomy"]]), 
            silent = T), references = try(xmlValue(dat[["INSDSeq_references"]]), 
            silent = T), feature_table = try(xmlValue(dat[["INSDSeq_feature-table"]]), 
            silent = T), qualifiers1 = readableFeatures, generegion = try(xmlValue(dat[["INSDSeq_feature-table"]][[2]][["INSDFeature_quals"]][["INSDQualifier"]][["INSDQualifier_value"]]), 
            silent = T), Full_sequence = ifelse(includeSeqs, 
            try(xmlValue(dat[["INSDSeq_sequence"]]), silent = T), 
            ""), authors = try(xmlValue(dat[["INSDSeq_references"]][[1]][["INSDReference_authors"]]), 
            silent = T))
        out <- c(out, featuresOutV)
        return(out)
    }
    if (!is.na(do[1])) 
        xmlMat <- t(mcmapply(get.a.row, xml_file$doc$children$INSDSet[do]))
    else xmlMat <- do.call(rbind, mclapply(seq(nRecords), get.a.row, 
        mc.cores = cores))
    if (parse.specimens) 
        xmlMat <- cbind(xmlMat, parse.specimen(xmlMat))
    return(xmlMat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
