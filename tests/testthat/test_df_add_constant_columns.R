context("df_add_constant_columns")

test_that("df_add_constant_columns", {
  x = data.frame(a = 1:2, b = 3:4)
  testthat::expect_error(df_add_constant_columns(x, list(a = "a")))

  ncs = list(c = "a", d = 0.1)
  y = df_add_constant_columns(x, ncs)
  checkmate::expect_data_frame(y, types = c("numeric", "numeric", "character", "numeric"),
    nrows = nrow(x), ncols = ncol(x) + length(ncs))
})
