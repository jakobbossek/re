context("addConstantColumns")

test_that("addConstantColumns", {
  x = data.frame(a = 1:2, b = 3:4)
  testthat::expect_error(addConstantColumns(x, list(a = "a")))

  ncs = list(c = "a", d = 0.1)
  y = addConstantColumns(x, ncs)
  checkmate::expect_data_frame(y, types = c("numeric", "numeric", "character", "numeric"),
    nrows = nrow(x), ncols = ncol(x) + length(ncs))
})
