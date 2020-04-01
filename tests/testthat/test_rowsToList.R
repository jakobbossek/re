context("rowsToList")

test_that("rowsToList", {
  x = data.frame(x = 1:3, y = letters[3:5])
  checkmate::expect_list(rowsToList(x), len = nrow(x))
  checkmate::expect_list(rowsToList(x, named = FALSE), len = nrow(x))
  checkmate::expect_list(rowsToList(unname(x), named = TRUE), len = nrow(x))
})
