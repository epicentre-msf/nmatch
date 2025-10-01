#' Compare sets of proper names accounting for common types of variation in
#' format and style, optimized using Rcpp
#'
#' @description
#' Compare proper names across two sources using string-standardization to
#' account for variation in punctuation, accents, and character case,
#' token-permutation to account for variation in name order, and fuzzy matching
#' to handle alternate spellings. The specific steps are:
#'
#' 1. Standardize strings. The default function is
#' \code{\link{name_standardize}} which removes accents and punctuation,
#' standardizes case, and removes extra whitespace. E.g. "Brontë, Emily J." is
#' standardized to "BRONTE EMILY J".
#'
#' 2. Tokenize standardized names, optionally retaining only tokens larger than
#' a given nchar limit.
#'
#' 3. For each pair of names, calculate string distance between all combinations
#' of tokens, and find the best overall token alignment (i.e. the alignment that
#' minimizes the summed string distance). If two names being compared differ in
#' their number of tokens, the alignment is made with respect to the smaller
#' number of tokens. E.g. If comparing "Angela Dorothea Merkel" to "Merkel
#' Angela", the token "Dorothea" would ultimately be omitted from the best
#' alignment.
#'
#' @inheritParams nmatch
#'
#' @return
#' Returns an integer matrix summarizing the match details, including columns:
#' - `k_x`: number of tokens in `x` (excludes tokens smaller than `nchar_min`)
#' - `k_y`: number of tokens in `y` (excludes tokens smaller than `nchar_min`)
#' - `k_align`: number of aligned tokens (i.e. `min(k_x, k_y)`)
#' - `dist_total`: summed string distance across aligned tokens
#' - `freq1`: summed frequency of first pair of aligned tokens (or NA if
#' argument token_freq not provided)
#' - `freq2`: summed frequency of first pair of aligned tokens (or NA if
#' argument token_freq not provided)
#' - `freq3`: summed frequency of first pair of aligned tokens (or NA if
#' argument token_freq not provided)
#'
#' @examples
#' names1 <- c(
#'   "Angela Dorothea Merkel",
#'   "Emmanuel Jean-Michel Fr\u00e9d\u00e9ric Macron",
#'   "Mette Frederiksen",
#'   "Katrin Jakobsd\u00f3ttir",
#'   "Pedro S\u00e1nchez P\u00e9rez-Castej\u00f3n"
#' )
#'
#' names2 <- c(
#'   "MERKEL, Angela",
#'   "MACRON, Emmanuel J.-M. F.",
#'   "FREDERICKSON, Mette",
#'   "JAKOBSDOTTIR  Kathríne",
#'   "PEREZ-CASTLEJON, Pedro"
#' )
#'
#' # return matrix with match details
#' nmatch_fast(names1, names2)
#'
#' @export nmatch_fast
nmatch_fast <- function(x,
                        y,
                        token_split = "[-_[:space:]]+",
                        nchar_min = 2L,
                        std = name_standardize,
                        ...,
                        token_freq = NULL) {

  ## match args
  if (!is.null(std)) {
    std <- match.fun(std)
  } else {
    std <- function(x) x
  }

  is_token_freq_null <- is.null(token_freq)

  if (is_token_freq_null) {
    token_freq <- data.frame(token = character(0), freq = integer(0))
  }

  ## standardize names
  x_std <- std(x, ...)
  y_std <- std(y, ...)

  ## call to cpp function
  out <- nmatch_cpp_tfreq(
    x_std,
    y_std,
    nchar_min,
    token = token_freq[[1]],
    token_freq = token_freq[[2]]
  )

  ## hack to deal with NA
  is_na_x <- is.na(x_std)
  is_na_y <- is.na(y_std)
  is_na_xy <- is_na_x | is_na_y
  is_k_align_zero <- out[,3] == 0L

  out[is_na_x, 1] <- NA_integer_
  out[is_na_y, 2] <- NA_integer_
  out[is_na_xy, 3] <- NA_integer_
  out[is_na_xy, 4] <- NA_integer_

  # if k_align = 0, force dist_total to NA (maybe be 9999 from nmatch_cpp_tfreq)
  out[is_k_align_zero, 4] <- NA_integer_

  ## return
  out
}

