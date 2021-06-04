#' @title
#' Subset check
#'
#' @description
#' Check if vector \code{x} is sub- or super-set of vector \code{y}.
#'
#' @param x [\code{vector}]\cr
#'   First vector.
#' @param y [\code{vector}]\cr
#'   Second vector.
#' @param strict [\code{logical}]
#' @return A single logical value.
#' @examples
#' x = c(1, 2, 3, 6, 7, 8)
#' y = 1:10
#' is_subset(x, y)
#' is_subset(y, x)
#' is_superset(x, y)
#' is_superset(y, x)
#' @export
is_subset = function(x, y, strict = FALSE) {
  checkmate::assert_vector(x, min.len = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_vector(y, min.len = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_flag(strict)
  iss = all(x %in% y)
  if (strict)
    return(iss && (length(unique(x)) < length(unique(y))))
  return(iss)
}

#' @rdname is_subset
#' @export
is_superset = function(x, y, strict = FALSE) {
  is_subset(y, x, strict)
}
