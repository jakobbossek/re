parseString = function(x, which = NULL, types, names, as.df = TRUE, ...) {
  checkmate::assertCharacter(x, min.len = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assertString(types)

  exploded = strsplit(x, ...)
  types = unlist(strsplit(types, ""))

  converter = c(
    "c" = as.character, "i" = as.integer,
    "n" = as.numeric, "l" = as.logical)

  ns = sapply(exploded, length)
  if (!all(ns == ns[1L])) {
    stopf("All splits need to be of the same length.")
  }

  if (is.null(which))
    which = seq_len(n.parts)
  checkmate::assertInteger(which, lower = 1, upper = ns[1L])
  n.parts = length(which)

  if (length(types) != n.parts) {
    stopf("Argument 'types' needs to have as many entries as there are selected parts.")
  }
  checkmate::assertSubset(types, choices = names(converter))

  #FIXME: write helper for that
  parts = do.call(rbind, exploded)
  parts = as.data.frame(parts)

  # filter
  parts = parts[, which, drop = FALSE]

  # now go through columns and convert to types
  for (i in seq_len(n.parts)) {
    tmp = parts[[i]]
    if (types[i] %in% c("i", "n", "l"))
      tmp = gsub("[a-zA-Z]", "", tmp) #FIXME: need to drop further symbols
    parts[[i]] = converter[[types[i]]](tmp)
  }
  colnames(parts) = names

  if (!as.df) {
    return(rowToList(parts, named = TRUE))
  }
  return(parts)
}
