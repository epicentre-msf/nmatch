#' Test version of nmatch_fast that evaluates string similarity scores
#'
#' @inheritParams nmatch
#'
#' @return
#' Returns a data frame summarizing the match details, including columns:
#' - `k_x`: number of tokens in `x` (excludes tokens smaller than `nchar_min`)
#' - `k_y`: number of tokens in `y` (excludes tokens smaller than `nchar_min`)
#' - `k_align`: number of aligned tokens (i.e. `min(k_x, k_y)`)
#' - `sim_total`: summed similarity score across aligned tokens (between 0 and
#' `k_align`, where `k_align` indicates a perfect match)
#' - `freq1`: summed frequency of first pair of aligned tokens (or NA if
#' argument `token_freq` not provided)
#' - `freq2`: summed frequency of second pair of aligned tokens (or NA if
#' argument `token_freq` not provided)
#' - `freq3`: summed frequency of third pair of aligned tokens (or NA if
#' argument `token_freq` not provided)
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
#' # return data frame with match details
#' nmatch_fast_similarity(names1, names2)
#'
#' @importFrom dplyr as_tibble
#' @export nmatch_fast_similarity
nmatch_fast_similarity <- function(
  x,
  y,
  token_split = "[-_[:space:]]+",
  nchar_min = 2L,
  std = name_standardize,
  ...,
  token_freq = NULL
) {
  ## match args
  if (!is.null(std)) {
    std <- match.fun(std)
  } else {
    std <- function(x) x
  }

  if (is.null(token_freq)) {
    token_freq <- data.frame(token = character(0), freq = integer(0))
  }

  ## standardize names
  x_std <- std(x, ...)
  y_std <- std(y, ...)

  ## call to cpp function
  out <- nmatch_cpp_similarity(
    x_std,
    y_std,
    nchar_min,
    token = token_freq[[1]],
    token_freq = token_freq[[2]]
  )

  ## handle NA inputs
  is_na_x <- is.na(x_std)
  is_na_y <- is.na(y_std)
  is_na_xy <- is_na_x | is_na_y

  out$k_x[is_na_x] <- NA_integer_
  out$k_y[is_na_y] <- NA_integer_
  out$k_align[is_na_xy] <- NA_integer_
  out$sim_total[is_na_xy] <- NA_real_

  ## return
  dplyr::as_tibble(out)
}
