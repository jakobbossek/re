sample2 = function(x, size, replace = FALSE, ...) {
  #FIXME: handle case where x is a scalar, size > 1 and replace = FALSE
  n = length(x)
  if ((n == 1L & size > 1L & !replace))
    stopf("Cannot sample %i elements from a set of cardinality 1 if replace=FALSE", size)
  if (n == 1L)
    return(rep(x, size))
  return(base::sample(x, size = size, replace = replace, ...))
}
