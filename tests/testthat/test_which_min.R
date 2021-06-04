context("which_min")

test_that("which_min", {
  x = c(10, 24, 2, 2, 15, 2, 28, 28, 2, 28)
  expect_true(which_min(x) == 3L)
  expect_true(which_min(x, "last") == 9L)
  checkmate::expect_set_equal(which_min(x, "all"), c(3, 4, 6, 9))
})
