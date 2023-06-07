#' Compare sets of proper names accounting for common types of variation in
#' format and style
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
#' 4. Summarize the number of tokens in each name, the number of tokens in the
#' best alignment, the number of aligned tokens that match (i.e. string
#' distance less than or equal to the defined threshold), and the summed string
#' distance of the best alignment.
#'
#' 5. Classify overall match status (TRUE/FALSE) based on match details
#' described in (4). By default, two names are considered to be matching if two
#' or more tokens match across names (e.g. "Merkel Angela" matches "Angela
#' Dorothea Merkel"), or if both names consist of only a single token which is
#' matching (e.g. "Beyonce" matches "Beyoncé").
#'
#' @param x,y Vectors of proper names to compare. Must be of same length.
#' @param token_split Regex pattern to split strings into tokens. Defaults to
#'   `"[-_[:space:]]+"`, which splits at each sequence of one more dash,
#'   underscore, or space character.
#' @param nchar_min Minimum token size to compare. Defaults to `2L`.
#' @param dist_method Method to use for string distance calculation (see
#'   \link[stringdist]{stringdist-metrics}). Defaults to `"osa"`.
#' @param dist_max Maximum string distance to use to classify matching tokens
#'   (i.e. tokens with a string distance less than or equal to `dist_max` will
#'   be considered matching). Defaults to `1L`.
#' @param std Function to standardize strings during matching. Defaults to
#'   \code{\link{name_standardize}}. Set to `NULL` to omit standardization.
#' @param ... additional arguments passed to `std()`
#' @param return_full Logical indicating whether to return data frame with full
#'   summary of match details (`TRUE`), or only a logical vector corresponding
#'   to final match status (`FALSE`). Defaults to `FALSE`.
#' @param eval_fn Function to determine overall match status. Defaults to
#'   \code{\link{match_eval}}. See section *Custom classification functions* for
#'   more details.
#' @param eval_params List of additional arguments passed to `eval_fn`
#'
#' @return
#' If `return_full = FALSE` (the default), returns a logical vector indicating
#' which elements of `x` and `y` are matches.
#'
#' If `return_full = TRUE`, returns a tibble-style data frame summarizing the
#' match details, including columns:
#' - `is_match`: logical vector indicating overall match status
#' - `k_x`: number of tokens in `x` (excludes tokens smaller than `nchar_min`)
#' - `k_y`: number of tokens in `y` (excludes tokens smaller than `nchar_min`)
#' - `k_align`: number of aligned tokens (i.e. `min(k_x, k_y)`)
#' - `n_match`: number of aligned tokens that match (i.e. distance <= `dist_max`)
#' - `dist_total`: summed string distance across aligned tokens
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
#' # return logical vector specifying which names are matches
#' nmatch(names1, names2)
#'
#' # increase the threshold string distance to allow for 'fuzzier' matches
#' nmatch(names1, names2, dist_max = 2)
#'
#' # return data frame with full match details
#' nmatch(names1, names2, return_full = TRUE)
#'
#' # use a custom function to classify matches
#' classify_matches <- function(k_align, n_match, dist_total, ...) {
#'   n_match == k_align & dist_total < 2
#' }
#'
#' nmatch(names1, names2, return_full = TRUE, eval_fn = classify_matches)
#'
#' @import dplyr
#' @importFrom purrr map map2_int
#' @importFrom stringdist stringdist
#' @importFrom rlang .data .env
#' @export nmatch
nmatch <- function(x,
                   y,
                   token_split = "[-_[:space:]]+",
                   nchar_min = 2L,
                   dist_method = "osa",
                   dist_max = 1L,
                   std = name_standardize,
                   ...,
                   return_full = FALSE,
                   eval_fn = match_eval,
                   eval_params = list(n_match_crit = 2)) {

  ## match args
  if (!is.null(std)) {
    std <- match.fun(std)
  } else {
    std <- function(x) x
  }

  eval_fn <- match.fun(eval_fn)

  ## string standardize x and y
  dat_std <- tibble(
    id = seq_along(x),
    x,
    y,
    x_std = std(x, ...),
    y_std = std(y, ...)
  )

  ## tokenize
  dat_tokens <- dat_std %>%
    mutate(
      x_token = purrr::map(.data$x_std, tokenize, prefix = "x", exclude_nchar = .env$nchar_min),
      y_token = purrr::map(.data$y_std, tokenize, prefix = "y", exclude_nchar = .env$nchar_min)
    ) %>%
    unnest_tokens(by = c("x_token", "y_token"))

  ## summarize number of tokens per name
  dat_token_counts <- dat_tokens %>%
    group_by(.data$id) %>%
    summarize(
      k_x = length(unique(.data$x_index)),
      k_y = length(unique(.data$y_index)),
      .groups = "drop"
    ) %>%
    mutate(
      k_align = purrr::map2_int(.data$k_x, .data$k_y, min)
    ) %>%
    left_join(x = dat_std, by = "id") %>%
    select(-any_of(c("x", "y", "x_std", "y_std")))

  ## calculate stringdist between tokens
  dat_tokens_dist <- dat_tokens %>%
    filter(nchar(.data$x_token) >= .env$nchar_min) %>%
    mutate(
      dist = stringdist::stringdist(.data$x_token, .data$y_token, method = dist_method),
      match = .data$dist <= .env$dist_max
    )

  ## summarize best-matching tokens and return
  match_summary <- dat_tokens_dist %>%
    group_by(.data$id, .data$x_index) %>%
    filter(.data$dist == min(.data$dist)) %>%
    ungroup() %>%
    arrange(.data$id, .data$y_token, .data$dist) %>%
    group_by(.data$id, .data$y_token) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(.data$id) %>%
    summarize(
      n_match = sum(.data$match),
      dist_total = sum(.data$dist),
      .groups = "drop"
    ) %>%
    left_join(x = dat_token_counts, by = "id") %>%
    select(!any_of("id"))

  ## evalutate whether overall match
  is_match <- do.call(
    eval_fn,
    c(as.list(match_summary), eval_params)
  )

  ## return either full match details or logical is_match
  if (return_full) {
    out <- match_summary %>%
      mutate(is_match = is_match) %>%
      select(all_of("is_match"), everything())
  } else {
    out <- is_match
  }

  out
}
