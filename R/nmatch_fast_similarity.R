#' Test version of nmatch_fast that evaluates a TF-IDF weighted evidence score
#'
#' @inheritParams nmatch
#'
#' @param token_idf_x data frame with columns `token` (character) and `idf`
#' (numeric) providing IDF weights for tokens in the `x`-side corpus. If
#' `NULL` (default), all `x` tokens receive equal weight (IDF = 1).
#' @param token_idf_y data frame with columns `token` (character) and `idf`
#' (numeric) providing IDF weights for tokens in the `y`-side corpus. If
#' `NULL` (default), all `y` tokens receive equal weight (IDF = 1).
#'
#' @return
#' Returns a data frame summarizing the match details, including columns:
#' - `k_x`: number of tokens in `x` (excludes tokens smaller than `nchar_min`)
#' - `k_y`: number of tokens in `y` (excludes tokens smaller than `nchar_min`)
#' - `k_align`: number of aligned tokens (i.e. `min(k_x, k_y)`)
#' - `similarity`: summed string similarity across aligned token pairs, computed
#' as `sum(1 - dist_i / max(nchar(x_i), nchar(y_i)))`
#' - `evidence`: TF-IDF weighted evidence score across aligned tokens,
#' computed as `sum(sim(x_i, y_i)^2 * (IDF_x_i + IDF_y_i) / 2)`, where
#' `sim` is the normalized OSA string similarity (between 0 and 1)
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
#' # return data frame with match details (uniform IDF weights)
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
  token_idf_x = NULL,
  token_idf_y = NULL
) {
  ## match args
  if (!is.null(std)) {
    std <- match.fun(std)
  } else {
    std <- function(x) x
  }

  if (is.null(token_idf_x)) {
    token_idf_x <- data.frame(token = character(0), idf = numeric(0))
    default_idf_x <- 1.0
  } else {
    default_idf_x <- max(token_idf_x[[2]], na.rm = TRUE)
  }

  if (is.null(token_idf_y)) {
    token_idf_y <- data.frame(token = character(0), idf = numeric(0))
    default_idf_y <- 1.0
  } else {
    default_idf_y <- max(token_idf_y[[2]], na.rm = TRUE)
  }

  ## standardize names
  x_std <- std(x, ...)
  y_std <- std(y, ...)

  ## call to cpp function
  out <- nmatch_cpp_similarity(
    x_std,
    y_std,
    nchar_min,
    token_x = token_idf_x[[1]],
    idf_x = token_idf_x[[2]],
    default_idf_x = default_idf_x,
    token_y = token_idf_y[[1]],
    idf_y = token_idf_y[[2]],
    default_idf_y = default_idf_y
  )

  ## handle NA inputs
  is_na_x <- is.na(x_std)
  is_na_y <- is.na(y_std)
  is_na_xy <- is_na_x | is_na_y

  out$k_x[is_na_x] <- NA_integer_
  out$k_y[is_na_y] <- NA_integer_
  out$k_align[is_na_xy] <- NA_integer_
  out$similarity[is_na_xy] <- NA_real_
  out$evidence[is_na_xy] <- NA_real_

  ## return
  dplyr::as_tibble(out)
}
