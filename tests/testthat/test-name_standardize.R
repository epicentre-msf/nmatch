test_that("name_standardize works as expected", {

  expect_equal(name_standardize("angela_merkel"), "ANGELA MERKEL")
  expect_equal(name_standardize("QUOIREZ, Fran\U00E7oise D."), "QUOIREZ FRANCOISE D")
})
