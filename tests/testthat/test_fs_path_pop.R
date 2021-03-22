context("fs_path_pop")

test_that("fs_path_pop works as expected", {
  x = "this/is/a/large/file.with.many.extensions"
  testthat::expect_equal(fs_path_pop(x), "file.with.many.extensions")
  testthat::expect_equal(fs_path_pop(x, 2L), "large/file.with.many.extensions")
  testthat::expect_equal(fs_path_pop(x, 3), "a/large/file.with.many.extensions")
})
