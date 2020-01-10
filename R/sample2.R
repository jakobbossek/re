sample2 = function(x, size, ...) {
  #FIXME: handle case where x is a scalar, size > 1 and replace = FALSE
  if (length(x) == 1)
    return(rep(x, size))
  return(base::sample(x, size = size, ...))
}
