#' Test version of nmatch_fast that explicitly incorporate false-positive probabilities
#' for every token pair in the best alignment
#'
#' @inheritParams nmatch
#'
#' @param token_match_prob_x data frame with columns `token` (character), `dist`
#' (integer), and `prob` (numeric) for the x-side corpus, as returned by
#' \code{\link{token_match_probs}}. If `NULL` (default), `p1`, `p2`, `p3`,
#' and `log_score` are `NA`.
#' @param token_match_prob_y data frame with columns `token` (character), `dist`
#' (integer), and `prob` (numeric) for the y-side corpus, as returned by
#' \code{\link{token_match_probs}}. If `NULL` (default), `p1`, `p2`, `p3`,
#' and `log_score` are `NA`.
#' @param token_idf_x data frame with columns `token` (character) and `idf`
#' (numeric) providing IDF weights for tokens in the `x`-side corpus, as
#' returned by \code{\link{token_idf}}. If `NULL` (default), `idf_score`
#' is `NA`.
#' @param token_idf_y data frame with columns `token` (character) and `idf`
#' (numeric) providing IDF weights for tokens in the `y`-side corpus, as
#' returned by \code{\link{token_idf}}. If `NULL` (default), `idf_score`
#' is `NA`.
#'
#' @return
#' Returns a data frame summarizing the match details, including columns:
#' - `k_x`: number of tokens in `x` (excludes tokens smaller than `nchar_min`)
#' - `k_y`: number of tokens in `y` (excludes tokens smaller than `nchar_min`)
#' - `k_align`: number of aligned tokens (i.e. `min(k_x, k_y)`)
#' - `n_match`: number of tokens that match (see \link{match_eval_token} for match logic)
#' - `dist_total`: summed string distance across aligned tokens
#' - `similarity`: summed string similarity across aligned token pairs, computed
#' as `sum(1 - dist_i / max(nchar(x_i), nchar(y_i)))`; ranges from 0 to `k_align`
#' - `log_score`: sum of `-log(geomean(p_x_i, p_y_i))` across all `k_align`
#' aligned token pairs
#' - `idf_score`: TF-IDF weighted similarity score across aligned tokens, computed
#' as `sum(similarity(x_i, y_i)^2 * (IDF_x_i + IDF_y_i) / 2)`
#'
#' The alignment is chosen to minimise summed string distance. `log_score` is
#' `NA` when `token_match_prob_x`/`token_match_prob_y` are not provided.
#' `idf_score` is `NA` when `token_idf_x`/`token_idf_y` are not provided.
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
  token_match_prob_x = NULL,
  token_match_prob_y = NULL,
  token_idf_x = NULL,
  token_idf_y = NULL
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

  ## unpack token prob data frames into vectors for C++
  if (is.null(token_match_prob_x)) {
    token_x <- character(0)
    dist_x <- integer(0)
    prob_x <- numeric(0)
  } else {
    token_x <- token_match_prob_x$token
    dist_x <- token_match_prob_x$dist
    prob_x <- token_match_prob_x$prob
  }

  if (is.null(token_match_prob_y)) {
    token_y <- character(0)
    dist_y <- integer(0)
    prob_y <- numeric(0)
  } else {
    token_y <- token_match_prob_y$token
    dist_y <- token_match_prob_y$dist
    prob_y <- token_match_prob_y$prob
  }

  ## prepare IDF args
  compute_idf_score <- !is.null(token_idf_x) || !is.null(token_idf_y)

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
    prob_y = prob_y,
    idf_token_x = token_idf_x[[1]],
    idf_x = token_idf_x[[2]],
    default_idf_x = default_idf_x,
    idf_token_y = token_idf_y[[1]],
    idf_y = token_idf_y[[2]],
    default_idf_y = default_idf_y,
    compute_idf_score = compute_idf_score
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
  out$log_score[is_na_xy] <- NA_real_
  out$log_score[is_k_align_zero] <- NA_real_
  out$idf_score[is_na_xy] <- NA_real_
  out$idf_score[is_k_align_zero] <- NA_real_

  ## return
  dplyr::as_tibble(out)
}
