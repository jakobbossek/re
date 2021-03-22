context("fs_dirname")

test_that("fs_dirname works as expected", {
  x = "this/is/a/large/file.with.many.extensions"
  testthat::expect_equal(fs_dirname(x), "this/is/a/large")
  testthat::expect_equal(fs_dirname(x, 2L), "this/is/a")
  testthat::expect_equal(fs_dirname(x, 3L), "this/is")
})
