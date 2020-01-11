context("sample2")

test_that("sample2", {
  testthat::expect_error(sample("a", 2L, replace = FALSE))
  testthat::expect_true(all(sample(c(0L, 1L), size = 10, replace = TRUE, prob = c(0, 1))) == 1L)
})
