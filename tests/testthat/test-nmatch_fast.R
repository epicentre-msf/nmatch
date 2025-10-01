test_that("nmatch_fast works as expected", {

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
  m1 <- nmatch_fast(x1, x2)
  expect_is(m1, "matrix")
  expect_type(m1, "integer")
  expect_equal(dim(m1), c(6L, 7L))

  # handling of missing values
  expect_equal(as.integer(nmatch_fast(NA, NA)), rep(NA_integer_, 7L))
  expect_equal(as.integer(nmatch_fast(NA, "blah blah")), c(NA, 2L, NA, NA, NA, NA, NA))
  expect_equal(as.integer(nmatch_fast("blah blah", NA)), c(2L, NA, NA, NA, NA, NA, NA))

  # arg nchar_min
  m2 <- nmatch_fast("charles abe smith", "SMITH CHARLES ABE", nchar_min = 3)
  m3 <- nmatch_fast("charles abe smith", "SMITH CHARLES ABE", nchar_min = 4)
  expect_equal(as.integer(m2), c(3L, 3L, 3L, 0L, NA, NA, NA))
  expect_equal(as.integer(m3), c(2L, 2L, 2L, 0L, NA, NA, NA))

  # test where k_align is 0
  m4 <- nmatch_fast("", "John Smith")
  expect_equal(as.integer(m4), c(0L, 2L, 0L, NA, NA, NA, NA))

  # test arg token_freq
  df_token_frequencies <- data.frame(
    x1 = name_standardize(c("charles", "smith", "smyth")),
    x2 = c(100L, 50L, 20L)
  )

  m5 <- nmatch_fast("charles abe smith", "SMYTH CHARLES ABE", token_freq = df_token_frequencies)
  expect_equal(as.integer(m5), c(3L, 3L, 3L, 1L, 200L, NA, 70))

})
