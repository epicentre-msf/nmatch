#' Test version of nmatch_fast that explicitly incorporate false-positive probabilities
#' for every token pair in the best alignment
#'
#' @inheritParams nmatch
#'
#' @param token_x Vector of standardized tokens for the `x`-side lookup table
#' @param dist_x Integer vector of OSA distances corresponding to each token in `token_x`
#' @param prob_x Numeric vector of probabilities corresponding to each token in `token_x`
#' @param token_y Vector of standardized tokens for the `y`-side lookup table
#' @param dist_y Integer vector of OSA distances corresponding to each token in `token_y`
#' @param prob_y Numeric vector of probabilities corresponding to each token in `token_y`
#'
#' @return
#' Returns a data frame summarizing the match details, including columns:
#' - `k_x`: number of tokens in `x` (excludes tokens smaller than `nchar_min`)
#' - `k_y`: number of tokens in `y` (excludes tokens smaller than `nchar_min`)
#' - `k_align`: number of aligned tokens (i.e. `min(k_x, k_y)`)
#' - `n_match`: number of tokens that match (see \link{match_eval_token} for match logic)
#' - `dist_total`: summed string distance across aligned tokens
#' - `p1`: false-positive match probability with respect to first aligned token pair
#' - `p2`: false-positive match probability with respect to second aligned token pair
#' - `p3`: false-positive match probability with respect to third aligned token pair
#' - `similarity`: summed string similarity across aligned token pairs, computed
#' as `sum(1 - dist_i / max(nchar(x_i), nchar(y_i)))`; ranges from 0 to `k_align`
#' - `weight`: sum of `-log(p_i)` across all `k_align` aligned token pairs
#'
#' The alignment is chosen to minimise summed string distance. `p1`, `p2`,
#' `p3`, and `weight` are `NA` when no token probability tables are provided.
#'
#' @importFrom dplyr as_tibble
#' @export nmatch_fast_prob
nmatch_fast_prob <- function(
  x,
  y,
  token_split = "[-_[:space:]]+",
  nchar_min = 2L,
  std = name_standardize,
  ...,
  token_x = NULL,
  dist_x = NULL,
  prob_x = NULL,
  token_y = NULL,
  dist_y = NULL,
  prob_y = NULL
) {
  ## match args
  if (!is.null(std)) {
    std <- match.fun(std)
  } else {
    std <- function(x) x
  }

  ## standardize names
  x_std <- std(x, ...)
  y_std <- std(y, ...)

  ## coerce NULLs to empty vectors for C++
  if (is.null(token_x)) {
    token_x <- character(0)
  }
  if (is.null(dist_x)) {
    dist_x <- integer(0)
  }
  if (is.null(prob_x)) {
    prob_x <- numeric(0)
  }
  if (is.null(token_y)) {
    token_y <- character(0)
  }
  if (is.null(dist_y)) {
    dist_y <- integer(0)
  }
  if (is.null(prob_y)) {
    prob_y <- numeric(0)
  }

  ## call to cpp function
  out <- nmatch_cpp_tprob(
    x_std,
    y_std,
    nchar_min,
    token_x = token_x,
    dist_x = dist_x,
    prob_x = prob_x,
    token_y = token_y,
    dist_y = dist_y,
    prob_y = prob_y
  )

  ## handle NA inputs
  is_na_x <- is.na(x_std)
  is_na_y <- is.na(y_std)
  is_na_xy <- is_na_x | is_na_y
  is_k_align_zero <- out[, 3] == 0L

  out[is_na_x, 1] <- NA_integer_
  out[is_na_y, 2] <- NA_integer_
  out[is_na_xy, 3] <- NA_integer_
  out[is_na_xy, 4] <- NA_integer_
  out[is_na_xy, 5] <- NA_integer_

  # if k_align = 0, force dist_total to NA (may be 9999 from nmatch_cpp_tprob)
  out[is_k_align_zero, 5] <- NA_integer_

  out$similarity[is_na_xy] <- NA_real_
  out$similarity[is_k_align_zero] <- NA_real_
  out$weight[is_na_xy] <- NA_real_
  out$weight[is_k_align_zero] <- NA_real_

  ## return
  dplyr::as_tibble(out)
}
