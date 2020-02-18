context("asNamedList")

test_that("asNamedList", {
  x = 1:3
  y = asNamedList(x, names = "x")
  checkmate::expect_names(names(y), type = "strict")
  checkmate::expect_list(y, types = "numeric")

  y = asNamedList(x, names = c("e1", "e2", "e3"))
  checkmate::expect_names(names(y), type = "strict")
  checkmate::expect_list(y, types = "numeric")

  testthat::expect_error(asNamedList(x, c("x1", "x2")))
})
