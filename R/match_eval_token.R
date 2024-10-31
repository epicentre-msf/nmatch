#' Evaluate string distance and token lengths to determine whether two tokens
#' match
#'
#' @param nchar_x Number of characters in token `x`
#' @param nchar_y Number of characters in token `y`
#' @param nchar_max `max(c(x_nchar, y_nchar))`
#' @param dist String distance between tokens x and y
#' @param ... Additional arguments (not used)
#'
#' @return
#' Logical vector indicating whether tokens `x` and `y` match, based on their
#' respective lengths and the string distance between them
#'
#' @export match_eval_token
match_eval_token <- function(nchar_x,
                             nchar_y,
                             nchar_max,
                             dist,
                             ...) {

  is_match <- (nchar_max <= 3 & dist == 0) |
    (nchar_max == 4 & dist <= 1) |
    (nchar_max >= 5 & nchar_max <= 8 & dist <= 2) |
    (nchar_max >= 9 & dist <= 3)

  is_match[is.na(is_match)] <- FALSE
  is_match
}

