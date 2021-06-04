context("is_subset")

test_that("is_{sub,super}set", {
  x = c(1, 2, 3, 8, 6)
  y = 1:10
  testthat::expect_true(is_subset(x, y))
  testthat::expect_true(is_superset(y, x))
  testthat::expect_false(is_subset(y, x))
  testthat::expect_true(is_subset(x, y, strict = TRUE))
  testthat::expect_true(is_superset(y, x, strict = TRUE))

  x = y = letters[1:10]
  testthat::expect_true(is_subset(x, y))
  testthat::expect_true(is_subset(y, x))
  testthat::expect_false(is_subset(x, y, strict = TRUE))
  testthat::expect_false(is_superset(y, x, strict = TRUE))
})
