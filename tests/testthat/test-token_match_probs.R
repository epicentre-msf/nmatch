x_corp <- c("Angela Merkel", "Pedro Sanchez", "Mette Frederiksen")
y_corp <- c("MERKEL Angela", "PEREZ Pedro", "FREDERICKSON Mette")

probs <- token_match_probs(x_corp, y_corp, verbose = FALSE)


test_that("token_match_probs returns correct structure", {
  expect_named(probs, c("x", "y"))
  expect_s3_class(probs$x, "tbl_df")
  expect_s3_class(probs$y, "tbl_df")
  expect_named(probs$x, c("token", "dist", "prob"))
  expect_named(probs$y, c("token", "dist", "prob"))
  expect_type(probs$x$token, "character")
  expect_type(probs$x$dist, "integer")
  expect_type(probs$x$prob, "double")
})


test_that("token_match_probs probabilities are valid", {
  # prob is in (0, 1]
  expect_true(all(probs$x$prob > 0 & probs$x$prob <= 1))
  expect_true(all(probs$y$prob > 0 & probs$y$prob <= 1))

  # max cumulative prob per token reaches exactly 1
  expect_true(all(tapply(probs$x$prob, probs$x$token, max) == 1))
  expect_true(all(tapply(probs$y$prob, probs$y$token, max) == 1))

  # dist values are non-negative
  expect_true(all(probs$x$dist >= 0L))
  expect_true(all(probs$y$dist >= 0L))

  # probs are non-decreasing within each token (sorted by dist)
  is_nondecreasing <- function(x) all(diff(x) >= 0)
  x_by_token <- split(probs$x$prob, probs$x$token)
  y_by_token <- split(probs$y$prob, probs$y$token)
  expect_true(all(sapply(x_by_token, is_nondecreasing)))
  expect_true(all(sapply(y_by_token, is_nondecreasing)))
})


test_that("token_match_probs tokens match corpus", {
  # tokens from x corpus appear in probs$x
  x_tokens <- unique(unlist(lapply(name_standardize(x_corp), tokenize_name)))
  expect_true(all(x_tokens %in% probs$x$token))

  # tokens from y corpus appear in probs$y
  y_tokens <- unique(unlist(lapply(name_standardize(y_corp), tokenize_name)))
  expect_true(all(y_tokens %in% probs$y$token))
})


test_that("token_match_probs batching gives consistent results", {
  p_batched   <- token_match_probs(x_corp, y_corp, batch_size = 1L,    verbose = FALSE)
  p_unbatched <- token_match_probs(x_corp, y_corp, batch_size = 1000L, verbose = FALSE)
  expect_equal(p_batched$x, p_unbatched$x)
  expect_equal(p_batched$y, p_unbatched$y)
})


test_that("token_match_probs nchar_min filters short tokens", {
  p <- token_match_probs(c("ab smith"), c("ab jones"), nchar_min = 3L, std = NULL, verbose = FALSE)
  expect_false("ab" %in% p$x$token)
  expect_true("smith" %in% p$x$token)
})


test_that("token_match_probs std = NULL skips standardization", {
  p <- token_match_probs(c("Angela"), c("ANGELA"), std = NULL, verbose = FALSE)
  expect_true("Angela" %in% p$x$token)
  expect_false("angela" %in% p$x$token)
})


test_that("token_match_probs handles NA names", {
  expect_no_error(
    token_match_probs(c("Angela Merkel", NA), c("MERKEL Angela", NA), verbose = FALSE)
  )
})


test_that("token_match_probs verbose controls messages", {
  expect_no_message(token_match_probs(x_corp, y_corp, verbose = FALSE))
  expect_message(token_match_probs(x_corp, y_corp, verbose = TRUE))
})


test_that("token_match_probs prob = 1 at dist 0 for identical single-token corpus", {
  p <- token_match_probs("merkel", "merkel", std = NULL, verbose = FALSE)
  prob_at_zero <- p$x$prob[p$x$token == "merkel" & p$x$dist == 0L]
  expect_equal(prob_at_zero, 1)
})
