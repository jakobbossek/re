#' @title Split strings into parts of certain types.
#'
#' @description Extract information from strings. This is done by splitting the
#' string by one or multiple delimiters. The split results are wrapped in a
#' \code{data.frame}. Optionally, one can pass column names and/or types for the
#' parts. E.g. \code{types="cir"} converts the first part into character, the
#' second into integer and the third into real/double.
#'
#' @param x [\code{character}]\cr
#'   Character vector.
#' @param split [\code{character(1)}]\cr
#'   See parameter \code{split} of \code{\link[base]{strsplit}}.
#' @param names [\code{character}]\cr
#'   Optional column names.
#'   If \code{NULL}, the column names are explode1 to explodeN where N is the number of
#'   split parts.
#' @param types [\code{character(1)}]\cr
#'   Character string where the i-th character specifies the atomic data type of
#'   the i-th split part. Possible characters are \dQuote{'c'} (character),
#'   \dQuote{'f'} (factor), dQuote{'i'} (integer), \dQuote{'l'} (logical) or
#'   \dQuote{n}, \dQuote{d} or \dQuote{r} for numeric/double/real.
#'   Default is \code{NULL}. In this case all split parts are characters.
#' @return [\code{data.frame}]
#' @export
str_explode = function(x, split, names = NULL, types = NULL) {
  checkmate::assert_character(x, min.len = 1L, any.missing = FALSE, all.missing = FALSE)

  exploded = strsplit(x, split = split)
  # take the length of the first item as ground truth
  n = length(exploded[[1L]])

  checkmate::assert_character(names, len = n, unique = TRUE, null.ok = TRUE)
  if (is.null(names))
    names = paste0("explode", seq_len(n))

  res = as.data.frame(do.call(rbind, exploded), stringsAsFactors = FALSE)
  colnames(res) = names

  if (!is.null(types)) {
    types = strsplit(types, split = "")[[1L]]
    checkmate::assert_character(types, len = n)
    converter_funs = type_shortcut_to_fun(types)
    for (i in seq_len(ncol(res))) {
      res[, i] = converter_funs[i][[1]](res[, i])
    }
  }
  return(res)
}

#' @rdname str_explode
#' @export
explode = function(x, split, names = NULL, types = NULL) {
  str_explode(x, split, names = NULL, types = NULL)
}

type_shortcut_to_fun = function(x) {
  mapping = c(
    "i" = as.integer,
    "c" = as.character,
    "f" = as.factor,
    "l" = as.logical,
    "d" = as.numeric,
    "r" = as.numeric)
  if (!(all(x %in% names(mapping))))
    re::stopf("[type_shortcut_to_fun] Types must be in {%s}.",
      re::collapse(paste0("'", names(mapping), "'"), sep = ", "))
  return(mapping[x])
}
