#' Evaluate token match details to determine overall match status
#'
#' @param k_x Integer vector specifying number of tokens in names `x`
#' @param k_y Integer vector specifying number of tokens in names `y`
#' @param n_match Integer vector specifying number of aligned tokens between x
#'   and y that are matching (i.e. based on argument `dist_max` in
#'   \code{\link{nmatch}})
#' @param n_match_crit Minimum number of matching tokens for names x and y to be
#'   considered an overall match
#' @param ... Additional arguments (not used)
#'
#' @return
#' Logical vector indicating whether names `x` and `y` match, based on the token
#' match details provided as arguments
#'
#' @export match_eval
match_eval <- function(k_x,
                       k_y,
                       n_match,
                       n_match_crit,
                       ...) {

  k_max <- mapply(max, k_x, k_y)

  is_match <- n_match == k_max | n_match >= n_match_crit
  is_match[is.na(is_match)] <- FALSE
  is_match
}

