#' Convert to named list
#'
#' @param x [\code{vector}]\cr
#'   Vector to be converted.
#' @param names [\code{character}]\cr
#'   Either a single character or vector of \code{length(x)} characters.
#' @param sep [\code{string}]\cr
#'   Single character used to split name from consecutive numbers in terms
#'   that \code{names} is a single character.
#' @return A named list.
#' @export
#' @examples
#' x = 1:3
#' as_named_list(x, names = "x", sep = "-")
#' as_named_list(x, names = "x")
#' as_named_list(x, names = c("e1", "e2", "e3"))
as_named_list = function(x, names, sep = ".") {
  checkmate::assert_vector(x, min.len = 1L)
  checkmate::assert_character(names, min.len = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_string(sep, na.ok = FALSE, min.chars = 1L)
  nx = length(x)
  nn = length(names)

  if ((nx != nn) & (nn > 1L))
    re::stopf("[as_named_list] names must be a character vector of length 1 or the length of x.")

  if (is.vector(x))
    x = as.list(x)
  if (nn == 1L)
    names = paste0(names, sep, seq_len(nx))
  names(x) = names
  return(x)
}
