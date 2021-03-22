context("as.named.list")

test_that("as.named.list", {
  x = 1:3
  y = as.named.list(x, names = "x")
  checkmate::expect_names(names(y), type = "strict")
  checkmate::expect_list(y, types = "numeric")

  y = as.named.list(x, names = c("e1", "e2", "e3"))
  checkmate::expect_names(names(y), type = "strict")
  checkmate::expect_list(y, types = "numeric")

  testthat::expect_error(as.named.list(x, c("x1", "x2")))
})
