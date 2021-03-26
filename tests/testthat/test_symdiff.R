context("symdiff")

test_that("symdiff", {
  x = letters[1:10]
  y = letters[3:12]
  checkmate::expect_set_equal(symdiff(x, y), c("a", "b", "k", "l"))
  checkmate::expect_set_equal(symdiff(x, y), symdiff(y, x))

  x = 20:30
  y = x + 5
  checkmate::expect_set_equal(symdiff(x, y), c(20:24, 31:35))
  checkmate::expect_set_equal(symdiff(x, y), symdiff(y, x))

  # disjoint
  x = 1:5
  y = 10:15
  checkmate::expect_set_equal(symdiff(x, y), union(x, y))

  # equal
  checkmate::expect_set_equal(symdiff(x, x), c())
})
