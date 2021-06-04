#' @title
#' Create a named list
#'
#' @description
#' Generator for named list (length corresponds to the number of
#' names) where each item is optionally initialized with a pre-defined value.
#'
#' @param ns [\code{character}]\cr
#'   Vector of list item names.
#' @param init [any]\cr
#'   Initial value for each list item.
#'   Defaults to \code{NULL}.
#' @return A named list.
#' @export
#' @examples
#' named_list(letters[1:10])
#' named_list(letters[1:10], init = NA)
#' named_list(letters[1:10], init = list())
#' named_list(letters[1:10], init = named_list(letters[1:2]))
named_list = function(ns, init = NULL) {
  checkmate::assert_character(ns, min.len = 1L, min.chars = 1L, any.missing = FALSE, all.missing = FALSE)
  n = length(ns)
  l = replicate(n, init, simplify = FALSE)
  names(l) = ns
  return(l)
}
