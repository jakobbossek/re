lapply.cbind = function(X, FUN, ...) {
  do.call(cbind, lapply(X, FUN, ...))
}

lapply.rbind = function(X, FUN, ...) {
  do.call(rbind, lapply(X, FUN, ...))
}
