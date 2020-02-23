context("as.data.frame2")

test_that("as.data.frame2", {
  ## LIST CONVERSION
  ## ===
  x = list(a = 1:2, b = c("K", "L"))
  y = as.data.frame2(x, col.names = NULL)

  # use names of list
  checkmate::expect_set_equal(names(x), colnames(y))

  # did not convert to factor
  checkmate::expect_character(y$b)

  # pass valid column names
  y = as.data.frame2(x, col.names = c("C1", "C2"))
  checkmate::expect_set_equal(colnames(y), c("C1", "C2"))

  # col.names contains NAs
  testthat::expect_error(as.data.frame2(x, col.names = c(1, NA)))
  # col.names type does not fit
  testthat::expect_error(as.data.frame2(x, col.names = 1:2))
  # col.names length does not fit
  testthat::expect_error(as.data.frame2(x, col.names = c("1", "2", "3")))

  ## MATRIX CONVERSION
  ## ===
  x = matrix(runif(50), ncol = 10)
  y = as.data.frame2(x, col.names = paste0("x", 1:10))
  testthat::expect_true(!is.null(colnames(y)))
})
