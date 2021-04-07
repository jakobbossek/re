context("insert")

test_that("insert", {
  x = list(a = 10, b = 4.5)

  y = insert(x, list(c = "char"))
  checkmate::expect_list(y, len = 3L)
  checkmate::expect_set_equal(names(y), c("a", "b", "c"))

  y = insert(x, list(c = "char", a = 15))
  checkmate::expect_list(y, len = 3L)
  checkmate::expect_set_equal(names(y), c("a", "b", "c"))
  expect_true(y$a == 15)

  y = insert(x, list())
  checkmate::expect_list(y, len = 2L)

  y = insert(x, list(a = 15, b = 1), which = "b")
  checkmate::expect_list(y, len = 2L)
  expect_true(y$a == 10)
  expect_true(y$b == 1)

  y = insert(x, list(a = 15, b = 1), which = c())
  checkmate::expect_list(y, len = 2L)
  expect_true(y$a == 10)
  expect_true(y$b == 4.5)
})
