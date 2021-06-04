#' @title
#' Set operation symmetric difference
#'
#' @description
#' Performs symmetric(!) \strong{set} difference in contrast
#' to \code{\link[base]{setdiff}}. I.e. given two vectors (of the same mode) the result
#' contains all elements that are either in the first or the second vector
#' but not in both. Duplicated values in the arguments will be discarded.
#'
#' @param x [\code{vector}]\cr
#'   Vector containing a sequence of items.
#' @param y [\code{vector}]\cr
#'   Another vector containing a sequence of items.
#' @return A vector with elements of the symmetric difference of \code{y} and \code{y}.
#' @export
#' @examples
#' symdiff(1:10, 6:15)
#' symdiff(1:10, 1:20)
#' symdiff(c("a", "b", "c"), c("d", "c", "c", "d", "e"))
symdiff = function(x, y) {
  c(setdiff(x, y), setdiff(y, x))
}
