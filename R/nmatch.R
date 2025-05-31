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
#' 4. For each pair of tokens in the best alignment, classify whether or not the
#' tokens match (TRUE/FALSE) based on their respective lengths and the string
#' distance between them.
#'
#' 5. Summarize the number of tokens in each name, the number of tokens in the
#' best alignment, the number of aligned tokens that match, and the summed
#' string distance of the best alignment.
#'
#' 6. Classify overall match status (TRUE/FALSE) based on the match details
#' described in (5). By default, two names are considered to be matching if two
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
#' @param std Function to standardize strings during matching. Defaults to
#'   \code{\link{name_standardize}}. Set to `NULL` to omit standardization.
#' @param ... additional arguments passed to `std()`
#' @param return_full Logical indicating whether to return data frame with full
#'   summary of match details (`TRUE`), or only a logical vector corresponding
#'   to final match status (`FALSE`). Defaults to `FALSE`.
#' @param return_alignment Logical indicating whether to return additional list
#'   column 'align' containing data frames with details on best alignment for
#'   each pair of names. Defaults to `FALSE`. Only used if `return_full = TRUE`.
#'   summary of match details (`TRUE`), or only a logical vector corresponding
#'   to final match status (`FALSE`). Defaults to `FALSE`.
#' @param eval_fn_token Function to determine token match status. Defaults to
#'   \code{\link{match_eval_token}}. See section *Custom classification
#'   functions* for more details.
#' @param eval_fn Function to determine overall match status. Defaults to
#'   \code{\link{match_eval}}. See section *Custom classification functions* for
#'   more details.
#' @param eval_params List of additional arguments passed to `eval_fn`
#' @param token_freq Optional data frame containing the frequencies of name
#'   tokens within the population of interest. Must have two columns
#'   - token_std: standardized tokens (using the same function as `std`)
#'   - freq: token frequency
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
#' @importFrom tidyr nest
#' @importFrom purrr map map2 map2_int
#' @importFrom stringdist stringdist
#' @importFrom rlang .data .env
#' @importFrom gtools permutations
#' @export nmatch
nmatch <- function(x,
                   y,
                   token_split = "[-_[:space:]]+",
                   nchar_min = 2L,
                   dist_method = "osa",
                   std = name_standardize,
                   ...,
                   return_full = FALSE,
                   return_alignment = FALSE,
                   eval_fn_token = match_eval_token,
                   eval_fn = match_eval,
                   eval_params = list(n_match_crit = 2),
                   token_freq = NULL) {


  ## match args
  if (!is.null(std)) {
    std <- match.fun(std)
  } else {
    std <- function(x) x
  }

  eval_fn <- match.fun(eval_fn)
  eval_fn_token <- match.fun(eval_fn_token)

  if (is.null(token_freq)) token_freq <- data.frame(token_std = character(0), freq = integer(0))

  ## string standardize x and y
  dat_std <- tibble(
    id = seq_along(x),
    x_std = std(x, ...),
    y_std = std(y, ...)
  )

  ## tokenize
  dat_tokens <- dat_std %>%
    mutate(
      x_token = purrr::map(.data$x_std, tokenize, exclude_nchar = .env$nchar_min),
      y_token = purrr::map(.data$y_std, tokenize, exclude_nchar = .env$nchar_min),
      x_index = purrr::map(.data$x_token, seq_along),
      y_index = purrr::map(.data$y_token, seq_along)
    ) %>%
    select(!all_of(c("x_std", "y_std"))) %>%
    unnest(c("x_token", "x_index")) %>%
    unnest(c("y_token", "y_index")) %>%
    mutate(
      rowid_temp = seq_len(n()),
      .before = 1
    )

  ## summarize number of tokens per name (excluding tokens shorter than nchar_min)
  dat_token_counts <- dat_tokens %>%
    group_by(.data$id) %>%
    summarize(
      k_x = max(.data$x_index),
      k_y = max(.data$y_index),
      .groups = "drop"
    ) %>%
    mutate(
      k_align = purrr::map2_int(.data$k_x, .data$k_y, min)
    ) %>%
    # make sure dat_token_counts contains all id
    # even for entries where all tokens excluded because shorter than nchar_min
    left_join(x = select(dat_std, id), by = "id")

  ## calculate stringdist between tokens
  is_na_x <- is.na(dat_tokens$x_token)
  is_na_y <- is.na(dat_tokens$y_token)
  dat_tokens <- dat_tokens[!is_na_x & !is_na_y, , drop = FALSE]
  # TODO: prevent fail if dat_tokens has no rows at this point
  # i.e. because all pairs have missing values or nchar < nchar_min

  dat_tokens$dist <- as.integer(
    stringdist::stringdist(
      dat_tokens$x_token,
      dat_tokens$y_token,
      method = dist_method
    )
  )

  ## find best alignment of tokens
  max_index <- max(c(dat_tokens$x_index, dat_tokens$y_index), na.rm = TRUE)

  perm_combos <- expand.grid(max = seq_len(max_index), min = seq_len(max_index)) %>%
    filter(min <= max) %>%
    arrange(max)

  perm_list <- purrr::map2(perm_combos$max, perm_combos$min, function(x1, x2) gtools::permutations(x1, x2))
  names(perm_list) <- paste(perm_combos$max, perm_combos$min, sep = "-")

  best_alignment <- dat_tokens %>%
    tidyr::nest(data = !id) %>%
    # within groups, must be arranged by x_index then y_index at this stage !
    mutate(rowid_temp_list = map(.data$data, find_best_alignment, perm_list = perm_list))

  match_summary_prep <- dat_tokens %>%
    filter(.data$rowid_temp %in% unlist(best_alignment$rowid_temp_list)) %>%
    left_join(select(token_freq, "token_std", freq_x = "freq"), by = c("x_token" = "token_std")) %>%
    left_join(select(token_freq, "token_std", freq_y = "freq"), by = c("y_token" = "token_std")) %>%
    mutate(
      freq_score = .data$freq_x + .data$freq_y,
      nchar_x = nchar(.data$x_token),
      nchar_y = nchar(.data$y_token),
      nchar_max = map2_int(.data$nchar_x, .data$nchar_y, max)
    )

  # cleanup
  rm(best_alignment, dat_tokens)

  if (return_alignment) {
    best_alignment_join <- match_summary_prep %>%
      select(id:dist) %>%
      tidyr::nest(align = !id)
  }

  # evalutate whether token match
  is_match_token <- do.call(
    eval_fn_token,
    c(as.list(match_summary_prep))
  )

  # prep output
  match_summary <- match_summary_prep %>%
    mutate(match = is_match_token) %>%
    group_by(.data$id) %>%
    summarize(
      n_match = sum(.data$match),
      dist_total = sum(.data$dist),
      freq_score = paste_collapse(.data$freq_score),
      .groups = "drop"
    ) %>%
    left_join(x = dat_token_counts, by = "id")

  # cleanup
  rm(match_summary_prep)

  # evalutate whether overall match
  is_match <- do.call(
    eval_fn,
    c(as.list(match_summary), eval_params)
  )

  ## return either full match details or logical is_match
  if (return_full) {
    out <- bind_cols(tibble(is_match = is_match), match_summary)

    if (return_alignment) {
      out <- out %>%
        left_join(best_alignment_join, by = "id")
    }

  } else {
    out <- is_match
  }

  out
}




#' @noRd
find_best_alignment <- function(x, perm_list) {
  # for each name x and y to match, we have previously calculated string
  # distance between all combinations of their tokens
  # here we find best alignment

  # indices may not be sequential after tokens shorter than nchar_min removed
  # this can lead errors (maybe silent errors too?) later in this fn
  # transforming to sequential here
  # x$x_index <- as.integer(as.factor(x$x_index))
  # x$y_index <- as.integer(as.factor(x$y_index))

  k_x <- max(x$x_index)
  k_y <- max(x$y_index)

  x_is_larger <- k_x > k_y
  k_min <- min(k_x, k_y)
  k_max <- max(k_x, k_y)

  if (all(is.na(x$x_token)) | all(is.na(x$y_token))) {

    out <- x$rowid_temp

  } else {

    dmat <- matrix(x$dist, nrow = k_x, byrow = T)

    if (x_is_larger) dmat <- t(dmat)

    p <- perm_list[[paste(k_max, k_min, sep = "-")]]

    # if !x_is_larger, row refers to x_token and col refers to y_token
    # if x_is_larger, row refers to y_token and col refers to x_token
    alignment_mats <- apply(p, 1, function (x) cbind(row = seq_len(k_min), col = x), simplify = FALSE)
    alignment_scores <- vapply(alignment_mats, function(x) sum(dmat[x]), 0)

    # note in case of ties which.min takes first
    best_alignment <- alignment_mats[[which.min(alignment_scores)]]
    if (x_is_larger) best_alignment <- best_alignment[,c(2, 1), drop = FALSE]
    colnames(best_alignment) <- c("x_index", "y_index")

    rows_best_alignment <- apply(
      best_alignment,
      1,
      function (rc) which(x$x_index %in% rc[1] & x$y_index %in% rc[2])
    )

    out <- x$rowid_temp[rows_best_alignment]
  }

  return(out)
}

