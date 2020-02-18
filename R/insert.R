insert = function(x, y, which) {
  if (length(y) == 0L)
    return(x)
  if (missing(which))
    which = names(y)
  else
    which = intersect(which, names(y))
  x[which] = y[which]
  return(x)
}
