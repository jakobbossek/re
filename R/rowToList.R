rowToList = function(x, named = TRUE) {
  checkmate::assertDataFrame(x, min.rows = 1L)
  lapply(seq_len(nrow(x)), function(i) as.list(x[i, ]))
}
