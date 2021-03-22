context("dirname2")

test_that("dirname2 works as expected", {
  x = "this/is/a/large/file.with.many.extensions"
  testthat::expect_equal(dirname2(x), "this/is/a/large")
  testthat::expect_equal(dirname2(x, 2L), "this/is/a")
  testthat::expect_equal(dirname2(x, 3L), "this/is")
})
