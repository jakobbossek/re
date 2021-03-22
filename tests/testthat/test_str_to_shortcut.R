context("str_to_shortcut")

test_that("str_to_shortcut", {
  x = c("one-nice-category", "second-category", "third")
  y = str_to_shortcut(x, split = "-", sep = "")
  checkmate::expect_set_equal(y, c("onc", "sc", "t"))
  y = str_to_shortcut(x, split = "-", nchars = 2)
  checkmate::expect_set_equal(y, c("on-ni-ca", "se-ca", "th"))
})
