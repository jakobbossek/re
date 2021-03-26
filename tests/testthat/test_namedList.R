context("namedList")

test_that("namedList", {
  ns = letters[sample(1:20, 5L)]
  l = namedList(ns)
  checkmate::expect_list(l, len = length(ns), names = "unique")
  checkmate::expect_set_equal(names(l), ns)
  expect_true(all(is.null(unlist(l))))

  l = namedList(ns, init = list("x" = 1))
  checkmate::expect_list(l, len = length(ns), names = "unique")
  checkmate::expect_set_equal(names(l), ns)
  expect_true(all(sapply(l, is.list)))
})
