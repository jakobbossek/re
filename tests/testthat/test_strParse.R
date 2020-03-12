context("strParse")

test_that("strParse", {
  x = c("char_int10_num10.4", "char_int28_num30.444")

  parsed = strParse(x, types = "cin", names = c("character", "integer", "numeric"), split = "_")
  checkmate::expect_data_frame(parsed, nrows = 2L, ncols = 4L, types = c("character", "integer", "numeric", "character"), col.names = "named", any.missing = FALSE, all.missing = FALSE)

  parsed = strParse(x, which = 2:3, types = "in", names = c("integer", "numeric"), append = FALSE, split = "_")
  checkmate::expect_data_frame(parsed, nrows = 2L, ncols = 2L, types = c("integer", "numeric"), col.names = "named", any.missing = FALSE, all.missing = FALSE)

  parsed = strParse(x, which = 2:3, types = "in", names = c("integer", "numeric"), as.df = FALSE, append = FALSE, split = "_")
  checkmate::expect_list(parsed, len = 2L, types = "list")
})
