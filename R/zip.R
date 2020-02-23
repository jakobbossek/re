#' @title Aggregate vectors/lists into lists of lists.
#'
#' @description This function emulates Python's zip function. It expects a
#' variable number of arguments. If a single vector/list x is passed the
#' return value is a list of lists where each sub-list contains the components
#' \dQuote{key} and \dQuote{value}. If x is named the key is the name, otherwise
#' it is the element number. The value field unsuprisingly contains the
#' corresponding value.
#'
#' @param ... [Lists/vectors]\cr
#'   A variable number of lists or vectors of the same size.
#' @return [\code{list}]
#' @examples
#' x = list("a" = 1, "b" = 2, "c" = 3)
#' for (el in zip(x)) {
#'   catf("Key: '%s', Value: %i\n", el$key, el$value)
#' }
#'
#' z = zip(x, x)
#' @export
zip = function(...) {
  args = list(...)
  n = length(args)
  if (n == 0)
    stopf("[zip] No arguments passed!")
  if (n == 1) {
    el = args[[1L]]
    nel = length(el)
    isNamed = checkmate::testNamed(el, type = "named")
    isList = is.list(el)
    ns = names(el)
    l = lapply(seq_len(nel), function(i) {
      list(key = if (isNamed) ns[i] else i, value = if (isList) el[[i]] else el[i])
    })
    return(l)
  } else {
    ns = sapply(args, length)
    args = lapply(args, unname)
    if (!all(ns == ns[1L]))
      BBmisc::stopf("[zip] All arguments need to have the same length.")
    l = vector(mode = "list", length = ns[1L])
    for (i in seq_len(ns[1L])) {
      l[[i]] = lapply(args, function(el) if (is.list(el)) el[[i]] else el[i])
    }
    return(l)
  }
}
