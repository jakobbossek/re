context("df_explode")

test_that("df_explode", {
  x = data.frame(a = c("x-y-1", "a-b-2", "c-d-3"), b = 1:3, c = runif(3))
  nr = nrow(x)
  nc = ncol(x)

  checkmate::test_data_frame(df_explode(x, split.col = "a", split = "-"),
    nrows = nr, ncols = nc + 3 - 1)

  checkmate::test_data_frame(df_explode(x, split.col = "a", split = "-", keep = TRUE),
    nrows = nr, ncols = nc + 3)

  xex = df_explode(x, split.col = "a", split = "-", keep = TRUE, names = c("A", "B", "C"))
  checkmate::test_data_frame(xex, nrows = nr, ncols = nc + 3)

  testthat::expect_true(all(colnames(xex) == c("A", "B", "C", "a", "b", "c")))

  xex = df_explode(x, split.col = "a", split = "-", keep = TRUE, types = "cci")
  checkmate::test_data_frame(xex, nrows = nr, ncols = nc + 3)

  checkmate::test_character(xex$explode1)
  checkmate::test_character(xex$explode2)
  checkmate::test_integer(xex$explode3)
})
