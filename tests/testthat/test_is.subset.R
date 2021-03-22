context("is.subset")

test_that("is.{sub,super}set", {
  x = c(1, 2, 3, 8, 6)
  y = 1:10
  testthat::expect_true(is.subset(x, y))
  testthat::expect_true(is.superset(y, x))
  testthat::expect_false(is.subset(y, x))
  testthat::expect_true(is.subset(x, y, strict = TRUE))
  testthat::expect_true(is.superset(y, x, strict = TRUE))

  x = y = letters[1:10]
  testthat::expect_true(is.subset(x, y))
  testthat::expect_true(is.subset(y, x))
  testthat::expect_false(is.subset(x, y, strict = TRUE))
  testthat::expect_false(is.superset(y, x, strict = TRUE))
})
