\name{label.elements}
\alias{label.elements}
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
label.elements(x, delim = "|", returnNum = 1, returnDelim = " ", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{delim}{
%%     ~~Describe \code{delim} here~~
}
  \item{returnNum}{
%%     ~~Describe \code{returnNum} here~~
}
  \item{returnDelim}{
%%     ~~Describe \code{returnDelim} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x, delim = "|", returnNum = 1, returnDelim = " ", ...)
{
    if ("phylo" \%in\% class(x))
        labelVector <- x$tip.label
    else labelVector <- x
    out <- sapply(labelVector, function(x) paste(strsplit(x,
        delim, ...)[[1]][returnNum], collapse = returnDelim))
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
