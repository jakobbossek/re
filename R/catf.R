#' @export
catf = function(fmt, ...) {
  cat(sprintf(fmt, ...))
}

#' @export
stopf = function(fmt, ...) {
  stop(sprintf(fmt, ...))
}

#' @export
warnf = function(fmt, ...) {
  warning(sprintf(fmt, ...))
}
