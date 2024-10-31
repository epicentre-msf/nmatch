
utils::globalVariables(c("."))


#' @noRd
#' @importFrom tidyr unnest
#' @importFrom dplyr all_of
unnest_tokens <- function(x, by) {
  for (j in by) { x <- tidyr::unnest(x, dplyr::all_of(j)) }
  x
}


#' @noRd
tokenize <- function(x, split = "[-_[:space:]]+", exclude_nchar) {
  token <- strsplit(x, split)[[1]]
  token <- token[nchar(token) >= exclude_nchar]
}


#' @noRd
paste_collapse <- function(x, collapse = "; ") {
  if (all(is.na(x))) {
    out <- NA_character_
  } else {
    out <- paste(x, collapse = collapse)
  }
  return(out)
}

