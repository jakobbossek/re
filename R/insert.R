#' @title Replace elements in a named vector or list.
#'
#' @description Given two named vectors/lists \code{x} and \code{y} and a list of names
#' this function replaces all entries of \code{x} with the matching entries in \code{y}.
#'
#' @param x [\code{vector}]\cr
#'   First vector.
#' @param y [\code{vector}]\cr
#'   Second vector.
#' @param which [\code{character}]\cr
#'   Entry names that should be replaced in \code{x}.
#'   Defaults to \code{names(y)}.
#' @return [\code{vector} | \code{list}]e
#' @examples
#' x = list(a = 100, b = letters[1:3])
#' y = list(b = "hello", c = "world")
#' insert(x, y)
#' insert(y, x)
#' insert(x, y, which = "b")
#' @export
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
