#' @title Formatted output.
#'
#' @description Simple wrappers around \code{cat}, \code{stop}, \code{warning}
#' and \code{sprintf}.
#'
#' @param fmt [\code{character(1)}]\cr
#'   Format string.
#' @param ... [any]\cr
#'   Further arguments passed down to \code{sprintf}.
#' @return Invisibly returns the return value of the respective function, e.g.
#' the character warning message in case of \code{warnf}.
#' @export
catf = function(fmt, ...) {
  cat(sprintf(fmt, ...))
}

#' @rdname catf
#' @export
stopf = function(fmt, ...) {
  stop(sprintf(fmt, ...))
}

#' @rdname catf
#' @export
warnf = function(fmt, ...) {
  warning(sprintf(fmt, ...))
}

#' @rdname catf
#' @aliases warnf
#' @export
warningf = function(fmt, ...) {
  warning(sprintf(fmt, ...))
}
