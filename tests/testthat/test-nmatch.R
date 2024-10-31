test_that("nmatch works as expected", {

  x1 <- c(
    "Beyonc\u00e9 Knowles",
    "Fr\u00e9d\u00e9ric Fran\u00e7ois Chopin",
    "Kendrick Lamar Duckworth",
    "Calvin Cordozar Broadus Jr.",
    "Céline Marie Claudette Dion",
    "Aubrey Drake Graham"
  )

  x2 <- c(
    "Beyonce Knowles-Carter",
    "CHOPIN, Fryderyk F.",
    "LAMAR, Kendrik",
    "Snoop Dogg",
    "DION, Céline",
    "Drake"
  )

  # basics
  m1 <- nmatch(x1, x2)
  expect_is(m1, "logical")
  expect_length(m1, length(x1))

  # expect more matches with more liberal token match fn
  match_token_cust <- function(dist, ...) { dist <= 3 }
  m2 <- nmatch(x1, x2, eval_fn_token = match_token_cust)
  expect_gt(sum(m2), sum(m1))

  # test arg return_full
  m3 <- nmatch(x1, x2, return_full = TRUE)
  expect_is(m3, "data.frame")
  expect_equal(m1, m3$is_match)

  # with no standardization expect fewer matches
  m4 <- nmatch(x1, x2, std = NULL)
  expect_lt(sum(m4), sum(m1))

  # with more liberal match function expect more matches
  match_cust <- function(k_align, dist_total, ...) {
    k_align > 0 & dist_total <= 3
  }

  m5 <- nmatch(x1, x2, eval_fn = match_cust)
  expect_gt(sum(m5), sum(m1))

})
