\name{tidyName}
\alias{tidyName}
\title{
Clean Up Strings
}
\description{
Remove spaces, punctuation, and capitalization from a string.
}
\usage{
tidyName(x, fixes = c("_", ".", " ", "-", ">"), case = c("lower", "upper", "nochange"))
}
\arguments{
  \item{x}{
The string to be modified.
}
  \item{fixes}{
Optional; a vector of all characters to be removed.
}
  \item{case}{
Optional; the case to which all characters in the string will be changed. There are three options: \code{lower} (the default), \code{upper}, and \code{nochange}.
}
}
\details{
By default, underscores (_), periods (.), spaces ( ), dashes (-) and right angle bracket (>) will be removed from the string, and the string will be converted to all lowercase.

This function was intended for use in standardizing voucher names in \code{make.unique.vouchers}, so that punctuation, spacing, and capitalization differences in sequence metadata does not prevent the grouping of multiple sequences to a single specimen voucher (ie: Hipp_2016 vs. HIPP2016).
}
\value{
The modified string is returned.
}
\author{
Andrew Hipp and Kasey Pham
}

\seealso{
\code{\link{make.unique.vouchers}}
}

\keyword{methods}
