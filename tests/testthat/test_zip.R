context("zip")

test_that("zip", {
  x = list("a" = 1, "b" = 2, "c" = 3)
  z = zip(x)
  checkmate::expect_list(z, types = "list", len = length(x), any.missing = FALSE, all.missing = FALSE)
  testthat::expect_true(z[[1]]$key == "a")
  testthat::expect_true(z[[1]]$value == 1)

  x = c("a", "b", "c")
  z = zip(x)
  checkmate::expect_list(z, types = "list", len = length(x), any.missing = FALSE, all.missing = FALSE)
  testthat::expect_true(z[[1]]$key == 1)
  testthat::expect_true(z[[1]]$value == "a")

  # Now multiple arguments
  x1 = list("a" = 1, "b" = 2, "c" = 3)
  x2 = list("x", "y", "z")
  x3 = 1:4
  testthat::expect_error(zip(x1, x2, x3))

  x3 = 1:3
  z = zip(x1, x2, x3)
  checkmate::expect_list(z, types = "list", len = 3L, any.missing = FALSE, all.missing = FALSE)
})
