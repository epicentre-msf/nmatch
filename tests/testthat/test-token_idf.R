x_corp <- c("Angela Merkel", "Pedro Sanchez", "Mette Frederiksen", "Angela Davis")
y_corp <- c("MERKEL Angela", "PEREZ Pedro", "FREDERICKSON Mette")

idfs <- token_idf(x_corp, y_corp)


test_that("token_idf returns correct structure", {
  expect_named(idfs, c("x", "y"))
  expect_s3_class(idfs$x, "tbl_df")
  expect_s3_class(idfs$y, "tbl_df")
  expect_named(idfs$x, c("token", "idf"))
  expect_named(idfs$y, c("token", "idf"))
  expect_type(idfs$x$token, "character")
  expect_type(idfs$x$idf, "double")
})


test_that("token_idf values are non-negative", {
  expect_true(all(idfs$x$idf >= 0))
  expect_true(all(idfs$y$idf >= 0))
})


test_that("token_idf idf = 0 for token appearing in every name", {
  p <- token_idf(
    c("john smith", "jane smith", "bob smith"),
    c("x"),
    std = NULL
  )

  # "smith" appears in all 3 names -> idf = log(3/3) = 0
  expect_equal(p$x$idf[p$x$token == "smith"], 0)
})


test_that("token_idf idf is correct for known document frequencies", {
  angela_idf <- idfs$x$idf[idfs$x$token == "ANGELA"]
  # ANGELA appears in 2 of 4 x names -> idf = log(4/2)
  expect_equal(angela_idf, log(4 / 2))

  merkel_idf <- idfs$x$idf[idfs$x$token == "MERKEL"]
  # MERKEL appears in 1 of 4 x names -> idf = log(4/1)
  expect_equal(merkel_idf, log(4 / 1))
})


test_that("token_idf tokens match corpus", {
  x_tokens <- unique(unlist(lapply(name_standardize(x_corp), tokenize_name)))
  expect_true(all(x_tokens %in% idfs$x$token))

  y_tokens <- unique(unlist(lapply(name_standardize(y_corp), tokenize_name)))
  expect_true(all(y_tokens %in% idfs$y$token))
})


test_that("token_idf nchar_min filters short tokens", {
  p <- token_idf(
    c("ab smith"),
    c("ab jones"),
    nchar_min = 3L,
    std = NULL
  )

  expect_false("ab" %in% p$x$token)
  expect_true("smith" %in% p$x$token)
})


test_that("token_idf std = NULL skips standardization", {
  p <- token_idf(c("Angela"), c("ANGELA"), std = NULL)
  expect_true("Angela" %in% p$x$token)
  expect_false("ANGELA" %in% p$x$token)
})


test_that("token_idf handles NA names", {
  expect_no_error(token_idf(c("Angela Merkel", NA), c("MERKEL Angela", NA)))
})


test_that("token_idf each token appears once per side", {
  expect_false(anyDuplicated(idfs$x$token) > 0)
  expect_false(anyDuplicated(idfs$y$token) > 0)
})
