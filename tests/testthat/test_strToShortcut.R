context("strToShortcut")

test_that("strToShortcut", {
  x = c("one-nice-category", "second-category", "third")
  y = strToShortcut(x, split = "-", sep = "")
  checkmate::expect_set_equal(y, c("onc", "sc", "t"))
  y = strToShortcut(x, split = "-", nchars = 2)
  checkmate::expect_set_equal(y, c("on-ni-ca", "se-ca", "th"))
})
