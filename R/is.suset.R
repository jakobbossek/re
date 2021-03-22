#' @title Subset check.
#'
#' @description Check if vector \code{x} is sub- or super-set of vector \code{y}.
#'
#' @param x [\code{vector}]\cr
#'   First vector.
#' @param y [\code{vector}]\cr
#'   Second vector.
#' @param strict [\code{logical}]
#' @return [\code{logical(1)}]
#' @examples
#' x = c(1, 2, 3, 6, 7, 8)
#' y = 1:10
#' is.subset(x, y)
#' is.subset(y, x)
#' is.superset(x, y)
#' is.superset(y, x)
#' @export
is.subset = function(x, y, strict = FALSE) {
  checkmate::assert_vector(x, min.len = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_vector(y, min.len = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_flag(strict)
  iss = all(x %in% y)
  if (strict)
    return(iss && (length(unique(x)) < length(unique(y))))
  return(iss)
}

#' @rdname is.subset
#' @export
is.superset = function(x, y, strict = FALSE) {
  is.subset(y, x, strict)
}
