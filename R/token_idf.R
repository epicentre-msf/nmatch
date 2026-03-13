#' Prepare token IDF tables for use with nmatch_fast_prob
#'
#' Computes per-token inverse document frequency (IDF) weights from two name
#' corpora. The returned tables can be passed directly to
#' \code{\link{nmatch_fast_prob}} as \code{token_idf_x} and
#' \code{token_idf_y}.
#'
#' For each token \code{t} in a corpus of \code{N} names, IDF is computed as:
#' \deqn{IDF(t) = \log(N / df(t))}
#' where \code{df(t)} is the number of names containing token \code{t} at
#' least once. Tokens that appear in every name receive IDF = 0; rare tokens
#' receive higher values.
#'
#' @inheritParams nmatch
#'
#' @param x Character vector of names from the x-side corpus
#' @param y Character vector of names from the y-side corpus
#'
#' @return A named list with two data frames:
#' - `x`: columns `token` (character) and `idf` (numeric) for the x-side corpus
#' - `y`: columns `token` (character) and `idf` (numeric) for the y-side corpus
#'
#' Pass these to \code{\link{nmatch_fast_prob}} as:
#' \preformatted{
#' idfs <- token_idf(corpus_x, corpus_y)
#' nmatch_fast_prob(
#'   x, y,
#'   token_idf_x = idfs$x,
#'   token_idf_y = idfs$y
#' )
#' }
#'
#' @importFrom dplyr as_tibble
#' @export token_idf
token_idf <- function(
  x,
  y,
  nchar_min = 2L,
  std = name_standardize,
  ...
) {
  if (!is.null(std)) {
    std <- match.fun(std)
  } else {
    std <- function(x) x
  }

  x_std <- std(x, ...)
  y_std <- std(y, ...)

  list(
    x = .token_idf_one(x_std, nchar_min),
    y = .token_idf_one(y_std, nchar_min)
  )
}


# Compute IDF for a single standardized name vector
.token_idf_one <- function(names, nchar_min) {
  names <- names[!is.na(names)]
  N <- length(names)

  # For each name, get unique tokens (IDF is based on document presence)
  token_lists <- lapply(names, function(nm) unique(tokenize_name(nm, nchar_min)))

  # Count number of names containing each token (document frequency)
  all_tokens <- unlist(token_lists)
  df <- as.integer(table(all_tokens))
  tok <- names(table(all_tokens))

  dplyr::tibble(
    token = tok,
    idf = log(N / df)
  )
}
