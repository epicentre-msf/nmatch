
#' @noRd
#' @importFrom tidyr unnest
#' @importFrom dplyr all_of
unnest_tokens <- function(x, by) {
  for (j in by) { x <- tidyr::unnest(x, dplyr::all_of(j)) }
  x
}


#' @noRd
#' @importFrom dplyr tibble
tokenize <- function(x, split = "[-_[:space:]]+", prefix, exclude_nchar) {
  token <- strsplit(x, split)[[1]]
  token <- token[nchar(token) >= exclude_nchar]
  index <- seq_along(token)
  out <- dplyr::tibble(index, token)
  names(out) <- paste(prefix, names(out), sep = "_")
  out
}

