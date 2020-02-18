context("dfAddCategory")

test_that("dfAddCategory", {
  x = data.frame(x = c(1:5), y = c("a", "a", "b", "b", "c"), stringsAsFactors = FALSE)
  y = dfAddCategory(x, "y", new.value = "abc")

  checkmate::expect_data_frame(y, nrows = 10)
  testthat::expect_true(all(table(y$y) == c(2, 5, 2, 1)))

  y = dfAddCategory(x, "y", new.value = "ac", values = c("a", "c"))
  checkmate::expect_data_frame(y, nrows = 8)
  testthat::expect_true(all(table(y$y) == c(2, 3, 2, 1)))

  x$x = factor(x$x, levels = 5:1, ordered = TRUE)
  x$y = factor(x$y, levels = c("b", "a", "c"), ordered = TRUE)

  y = dfAddCategory(x, "x", new.value = "union")
  checkmate::expect_factor(y$x, levels = c(5:1, "union"), ordered = TRUE)
})
