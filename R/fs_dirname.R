#' @title k-fold dirname.
#'
#' @description Given paths and an integer depth, the function calculates
#' dirname(...(...dirname(paths))), depth times.
#' @param path [\code{character}]\cr
#'   Paths.
#' @param depth [\code{integer(1)}]\cr
#'   Depth of the k-fold
#' @return [\code{character}]
#' @export
dirname2 = function(path, depth = 1L) {
  checkmate::assert_character(path)
  depth = checkmate::asInt(depth, lower = 1L)
  while (depth > 0L) {
    path = dirname(path)
    depth = depth - 1L
  }
  return(path)
}
