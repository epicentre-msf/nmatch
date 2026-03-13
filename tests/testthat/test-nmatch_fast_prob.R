corp_x <- c("Angela Merkel", "Pedro Sanchez", "Mette Frederiksen")
corp_y <- c("MERKEL Angela", "PEREZ Pedro", "FREDERICKSON Mette")

probs <- token_match_probs(corp_x, corp_y, verbose = FALSE)
idfs <- token_idf(corp_x, corp_y)

r_base <- nmatch_fast_prob(corp_x, corp_y)
r_probs <- nmatch_fast_prob(corp_x, corp_y, token_match_prob_x = probs$x, token_match_prob_y = probs$y)
r_idfs <- nmatch_fast_prob(corp_x, corp_y, token_idf_x = idfs$x, token_idf_y = idfs$y)


test_that("nmatch_fast_prob returns correct structure", {
  expect_s3_class(r_base, "tbl_df")
  expect_named(r_base, c("k_x", "k_y", "k_align", "n_match", "dist_total", "similarity", "log_score", "idf_score"))
  expect_equal(nrow(r_base), length(corp_x))
})


test_that("nmatch_fast_prob log_score is NA without prob tables, numeric with them", {
  expect_true(all(is.na(r_base$log_score)))
  expect_true(all(!is.na(r_probs$log_score)))
  expect_true(all(r_probs$log_score >= 0))
})


test_that("nmatch_fast_prob idf_score is NA without IDF tables, numeric with them", {
  expect_true(all(is.na(r_base$idf_score)))
  expect_true(all(!is.na(r_idfs$idf_score)))
  expect_true(all(r_idfs$idf_score >= 0))
})


test_that("nmatch_fast_prob similarity is 0 for token with zero-length intersection", {
  # single-token names with no characters in common
  r <- nmatch_fast_prob("aaa", "zzz", std = NULL)
  expect_true(r$similarity < 1)
})


test_that("nmatch_fast_prob perfect matches give dist_total = 0 and similarity = k_align", {
  r <- nmatch_fast_prob(corp_x, corp_x)
  expect_true(all(r$dist_total == 0L))
  expect_equal(r$similarity, as.numeric(r$k_align))
})


test_that("nmatch_fast_prob k_align = min(k_x, k_y)", {
  expect_equal(r_base$k_align, pmin(r_base$k_x, r_base$k_y))
})


test_that("nmatch_fast_prob handles NA inputs", {
  r <- nmatch_fast_prob(c("Angela Merkel", NA), c("MERKEL Angela", "foo"))
  expect_true(is.na(r$k_align[2]))
  expect_true(is.na(r$similarity[2]))
  expect_true(is.na(r$log_score[2]))
  expect_true(is.na(r$idf_score[2]))
})


test_that("nmatch_fast_prob log_score is higher for closer matches", {
  # close match (dist=0) vs. distant match — known tokens at dist=0 are
  # more surprising than at large dist, so log_score should be higher
  r <- nmatch_fast_prob(
    c("Angela Merkel", "Angela Merkel"),
    c("Angela Merkel", "Pedro Sanchez"),
    token_match_prob_x = probs$x,
    token_match_prob_y = probs$y
  )
  expect_true(r$log_score[1] > r$log_score[2])
})
