#' @title Test for permutation.
#'
#' @description Check whether \code{x} is a permutation of the multi-set \code{s}.
#'
#' @param x [\code{vector}]\cr
#'   Input vector to be checked.
#' @param s [\code{vector}]\cr
#'   Base set.
#' @examples
#' is.permutation(shuffle(1:10), 1:10)
#' is.permutation(c(2, 1, 4), s = 1:4)
#' @return [\code{logical(1)}]
#' @export
is.permutation = function(x, s) {
  checkmate::assert_vector(x)
  checkmate::assert_vector(s)
  all(x %in% s) & (length(x) == length(s))
}
