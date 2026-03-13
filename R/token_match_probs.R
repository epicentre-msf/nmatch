#' Prepare token match probability lookup tables for use with nmatch_fast_prob
#'
#' Computes per-token cumulative false-positive match probabilities by
#' cross-joining all token pairs from two name corpora, computing their OSA
#' string distances, and deriving empirical cumulative distributions. The
#' returned tables can be passed directly to \code{\link{nmatch_fast_prob}}.
#'
#' For each token `t` in a corpus, the cumulative probability at distance `d`
#' is `P(dist(t, t') <= d)`, where `t'` is a token drawn at random from the
#' opposite corpus (weighted by token frequency). This represents the
#' false-positive rate: how likely is it to observe a match this good by
#' chance?
#'
#' @inheritParams nmatch
#'
#' @param x Character vector of names from the x-side corpus
#' @param y Character vector of names from the y-side corpus
#' @param batch_size Number of unique x-side tokens to process per batch.
#'   Reducing this lowers peak memory at the cost of slightly more overhead.
#'   Defaults to \code{1000L}.
#' @param verbose If \code{TRUE} (default), prints progress after each batch.
#'
#' @return A named list with two data frames:
#' - `x`: columns `token` (character), `dist` (integer), `prob` (numeric) for
#'   the x-side corpus
#' - `y`: columns `token` (character), `dist` (integer), `prob` (numeric) for
#'   the y-side corpus
#'
#' Each row gives the cumulative probability `P(observed distance <= dist |
#' token)`. Pass these to \code{\link{nmatch_fast_prob}} as:
#' \preformatted{
#' probs <- token_match_probs(corpus_x, corpus_y)
#' nmatch_fast_prob(
#'   x, y,
#'   token_x = probs$x$token, dist_x = probs$x$dist, prob_x = probs$x$prob,
#'   token_y = probs$y$token, dist_y = probs$y$dist, prob_y = probs$y$prob
#' )
#' }
#'
#' @importFrom dplyr summarise arrange mutate transmute select as_tibble
#' @export token_match_probs
token_match_probs <- function(
  x,
  y,
  nchar_min = 2L,
  std = name_standardize,
  ...,
  batch_size = 1000L,
  verbose = TRUE
) {
  if (!is.null(std)) {
    std <- match.fun(std)
  } else {
    std <- function(x) x
  }

  x_std <- std(x, ...)
  y_std <- std(y, ...)

  freq_x <- .token_freq(x_std, nchar_min)
  freq_y <- .token_freq(y_std, nchar_min)

  n_x <- nrow(freq_x)
  batch_starts <- seq(1L, n_x, by = batch_size)
  n_batches <- length(batch_starts)

  agg_x <- NULL
  agg_y <- NULL

  for (b in seq_along(batch_starts)) {
    start <- batch_starts[b]
    end <- min(start + batch_size - 1L, n_x)
    fx_batch <- freq_x[start:end, ]

    pairs <- tidyr::expand_grid(
      ix = seq_len(nrow(fx_batch)),
      iy = seq_len(nrow(freq_y))
    ) |>
      transmute(
        token_x = fx_batch$token[.data$ix],
        token_y = freq_y$token[.data$iy],
        freq = fx_batch$freq[.data$ix] * freq_y$freq[.data$iy],
        dist = as.integer(stringdist::stringdist(.data$token_x, .data$token_y, method = "osa"))
      )

    agg_x <- rbind(agg_x, summarise(pairs, .by = c("token_x", "dist"), freq = sum(.data$freq)))
    agg_y <- rbind(agg_y, summarise(pairs, .by = c("token_y", "dist"), freq = sum(.data$freq)))

    if (verbose) {
      message("Batch ", b, " of ", n_batches, " complete (x tokens ", start, "-", end, " of ", n_x, ")")
    }
  }

  # agg_x: x tokens are unique across batches, no re-summarise needed
  # agg_y: same y tokens accumulate across batches, so consolidate here
  agg_y <- summarise(agg_y, .by = c("token_y", "dist"), freq = sum(.data$freq))

  list(
    x = as_tibble(.cumprob_from_agg(agg_x, "token_x")),
    y = as_tibble(.cumprob_from_agg(agg_y, "token_y"))
  )
}


# Tokenize a vector of names and return a data frame of token frequencies
.token_freq <- function(names, nchar_min) {
  tokens <- unlist(lapply(names[!is.na(names)], tokenize_name, nchar_min))
  tbl <- sort(table(tokens), decreasing = TRUE)
  data.frame(
    token = names(tbl),
    freq = as.integer(tbl),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


# Compute cumulative probabilities from a (token_col, dist, freq) aggregation data frame
.cumprob_from_agg <- function(agg, token_col) {
  agg$token <- agg[[token_col]]
  agg <- arrange(agg, .data$token, .data$dist)
  agg <- mutate(agg, .by = "token", prob = cumsum(.data$freq) / sum(.data$freq))
  select(agg, "token", "dist", "prob")
}
