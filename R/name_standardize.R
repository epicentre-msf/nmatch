#' String standardization
#'
#' Standardize strings prior to performing a match, using the following
#' transformations:
#' 1. standardize case (`base::toupper`)
#' 2. remove accents/diacritics (`stringi::stri_trans_general`)
#' 3. replace punctuation characters with whitespace
#' 4. remove extraneous space characters with (`stringr::str_squish`)
#'
#' @param x a string
#'
#' @return
#' The standardized version of `x`
#'
#' @examples
#' name_standardize("angela_merkel")
#' name_standardize("QUOIREZ, Fran\U00E7oise D.")
#'
#' @importFrom stringr str_squish
#' @importFrom stringi stri_trans_general
#' @export name_standardize
name_standardize <- function(x) {
  x <- toupper(x)
  x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
  x <- gsub("[[:punct:]]+", " ", x)
  x <- stringr::str_squish(x)
  x
}
