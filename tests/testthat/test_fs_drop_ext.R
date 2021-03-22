context("fs_drop_ext")

test_that("fs_drop_ext works as expected", {
  x = "this/is/a/file.with.many.extensions"
  testthat::expect_equal(fs_drop_ext(x), "this/is/a/file")
  testthat::expect_equal(fs_drop_ext(x, pos = "last"), "this/is/a/file.with.many")
  testthat::expect_equal(fs_drop_ext(x, pos = 3L), "this/is/a/file.with.many")
  testthat::expect_equal(fs_drop_ext(x, pos = 2L), "this/is/a/file.with")
  testthat::expect_equal(fs_drop_ext(x, pos = 1), "this/is/a/file")
})
