context("nunique")

test_that("nunique", {
  x = c(1, 2, rep(3, 10), 8)
  testthat::expect_equal(nunique(x), 4)

  # check factors (here nlevels(x) is 4)
  x = factor(c("a", "b", "b", "d"), levels = letters[1:4])
  testthat::expect_equal(nunique(x), 3)

  # With NA (NA is treated as )
  x[3] = NA
  testthat::expect_equal(nunique(x), 4)

  # fails on non-atomic input
  x = list(1, 2, 3)
  testthat::expect_error(nunique(x))
})
