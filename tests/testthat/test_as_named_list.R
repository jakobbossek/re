context("as_named_list")

test_that("as_named_list", {
  x = 1:3
  y = as_named_list(x, names = "x")
  checkmate::expect_names(names(y), type = "strict")
  checkmate::expect_list(y, types = "numeric")

  y = as_named_list(x, names = c("e1", "e2", "e3"))
  checkmate::expect_names(names(y), type = "strict")
  checkmate::expect_list(y, types = "numeric")

  testthat::expect_error(as_named_list(x, c("x1", "x2")))
})
