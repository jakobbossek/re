#' @title Parse strings
#'
#' @description Split character vector, i.e. each element, by a seperator and convert the resulting
#' exploded string into a typed list / data frame of meta information.
#'
#' @param x [\code{character}]\cr
#'   Input strings.
#' @param ext [\code{character(1) | NULL}]\cr
#'   Optional file extension which should be dropped prior to splitting.
#' @param which [\code{integer}]\cr
#'   Integer vector of relevant positions, i.e. positions in the exploded string which
#'   are of interest. Defaults to all positions.
#' @param types [\code{character(1)}]\cr
#'   Single character where the letter at position i characterizes the type of
#'   the corresponding element in the exploded string.
#'   Possible letters are \dQuote{c} (character), \dQuote{n} (numeric), \dQuote{i}
#'   (integer) or \dQuote{l} (logical).
#' @param names [\code{character}]\cr
#'   Vector of names for the extracted components.
#' @param as.df [\code{logical(1)}]\cr
#'   Should the result be a data frame?
#'   Default is \code{TRUE}. If \code{FALSE} a list of named lists is returned.
#' @param append [\code{logical(1)}]\cr
#'   Should the values of \code{x} be dropped or appended to the ouptut via
#'   component \dQuote{input}?
#'   Default is \code{TRUE}.
#' @param ... [any]\cr
#'   Further arguments passed down to \code{\link[base]{strsplit}}.
#'   Here, argument \code{split} is the most interesting one.
#' @return [\code{data.frame | list}]
#' @examples
#' x = c("char_int10_num10.4", "char_int28_num30.444")
#' str_parse(x, types = "cin", names = c("character", "integer", "numeric"), split = "_")
#' str_parse(x, which = 2:3, types = "in", names = c("integer", "numeric"), split = "_")
#' str_parse(x, which = 2:3, types = "in", names = c("integer", "numeric"),
#'   as.df = FALSE, append = FALSE, split = "_")
#' @export
str_parse = function(x, ext = NULL, which = NULL, types, names, as.df = TRUE, append = TRUE, ...) {
  checkmate::assertCharacter(x, min.len = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assertString(types)

  xbck = x
  if (!is.null(ext))
    x = gsub(ext, "", x, fixed = TRUE)

  exploded = strsplit(x, ...)
  types = unlist(strsplit(types, ""))

  converter = c(
    "c" = as.character, "i" = as.integer,
    "n" = as.numeric, "l" = as.logical)

  ns = sapply(exploded, length)
  if (!all(ns == ns[1L])) {
    stopf("All splits need to be of the same length.")
  }

  if (is.null(which))
    which = seq_len(ns[1L])
  checkmate::assertInteger(which, lower = 1, upper = ns[1L])
  n.parts = length(which)

  if (length(types) != n.parts) {
    stopf("Argument 'types' needs to have as many entries as there are selected parts.")
  }
  checkmate::assertSubset(types, choices = names(converter))

  #FIXME: write helper for that
  parts = do.call(rbind, exploded)
  parts = as.data.frame(parts)

  # filter
  parts = parts[, which, drop = FALSE]

  # now go through columns and convert to types
  for (i in seq_len(n.parts)) {
    tmp = parts[[i]]
    if (types[i] %in% c("i", "n", "l"))
      tmp = gsub("[a-zA-Z]", "", tmp) #FIXME: need to drop further symbols
    parts[[i]] = converter[[types[i]]](tmp)
  }
  colnames(parts) = names

  if (append)
    parts$input = xbck

  if (!as.df) {
    return(df_rows_to_list(parts, named = TRUE))
  }
  return(parts)
}

# Unexported function to make old scripts work
parseString = function(...) {
  str_parse(...)
}
