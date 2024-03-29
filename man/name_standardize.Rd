% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/name_standardize.R
\name{name_standardize}
\alias{name_standardize}
\title{String standardization}
\usage{
name_standardize(x)
}
\arguments{
\item{x}{a string}
}
\value{
The standardized version of \code{x}
}
\description{
Standardize strings prior to performing a match, using the following
transformations:
\enumerate{
\item standardize case (\code{base::toupper})
\item remove accents/diacritics (\code{stringi::stri_trans_general})
\item replace punctuation characters with whitespace
\item remove extraneous space characters with (\code{stringr::str_squish})
}
}
\examples{
name_standardize("angela_merkel")
name_standardize("QUOIREZ, Fran\U00E7oise D.")

}
