context("named_list")

test_that("named_list", {
  ns = letters[sample(1:20, 5L)]
  l = named_list(ns)
  checkmate::expect_list(l, len = length(ns), names = "unique")
  checkmate::expect_set_equal(names(l), ns)
  expect_true(all(is.null(unlist(l))))

  l = named_list(ns, init = list("x" = 1))
  checkmate::expect_list(l, len = length(ns), names = "unique")
  checkmate::expect_set_equal(names(l), ns)
  expect_true(all(sapply(l, is.list)))
})
