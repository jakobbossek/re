#' @title Conversion to data frame
#'
#' @description Wrapper around \code{\link[base]{as.data.frame}}.
#' In contrast it sets \code{stringsAsFactors=FALSE} and allows to
#' pass a vectors of column names.
#'
#' @param x [\code{object}]\cr
#'   Any R object that can be coerced into a data frame.
#' @param col.names [\code{character} | \code{NULL}]\cr
#'   Optional character vector of column names.
#' @param ... [\code{any}]\cr
#'   Further arguments passed down to \code{\link[base]{as.data.frame}}.
#' @return [\code{data.frame}]
#' @examples
#' x = list(a = 1:2, b = c("K", "L"))
#' y = as.data.frame2(x, col.names = c("C1", "C2"))
#' @export
as.data.frame2 = function(x, col.names = NULL, ...) {
  x = as.data.frame(x, stringsAsFactors = FALSE, ...)
  if (!is.null(col.names)) {
    checkmate::assertCharacter(col.names, len = ncol(x), any.missing = FALSE, all.missing = FALSE)
    colnames(x) = col.names
  }
  return(x)
}
