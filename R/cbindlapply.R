#' @title Bind lapply results.
#'
#' @description Simple shortcut for \code{do.call(cbind, lapply(...))}.
#'
#' @param X [any]\cr
#'   Iterable objects, e.g. list or vector.
#' @param FUN [\code{function}]\cr
#'   Function applied to each element of \code{X} (see \code{\link[base]{lapply}}).
#' @param ... [any]\cr
#'   Furhter parameters passed down to \code{\link[base]{lapply}}.
#' @return See return value of \code{\link[base]{cbind}} and \code{\link[base]{rbind}}
#' respectively.\cr
#' @export
cbindlapply = function(X, FUN, ...) {
  do.call(cbind, lapply(X, FUN, ...))
}

#' @rdname cbindlapply
#' @export
rbindlapply = function(X, FUN, ...) {
  do.call(rbind, lapply(X, FUN, ...))
}
