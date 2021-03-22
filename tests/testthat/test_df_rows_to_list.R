context("df_rows_to_list")

test_that("df_rows_to_list", {
  x = data.frame(x = 1:3, y = letters[3:5])
  checkmate::expect_list(df_rows_to_list(x), len = nrow(x))
  checkmate::expect_list(df_rows_to_list(x, named = FALSE), len = nrow(x))
  checkmate::expect_list(df_rows_to_list(unname(x), named = TRUE), len = nrow(x))
})
