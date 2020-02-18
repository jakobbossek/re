catf = function(fmt, ...) {
  cat(sprintf(fmt, ...))
}

stopf = function(fmt, ...) {
  stop(sprintf(fmt, ...))
}

warnf = function(fmt, ...) {
  warning(sprintf(fmt, ...))
}
