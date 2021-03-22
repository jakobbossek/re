#' @title Make string shortcuts.
#'
#' @description Given a character vector, each element is split, the first
#' \code{nchars} characters are extracted from each part and all parts are
#' concatenated with \code{sep}.
#' This is useful to obtain shortcuts of lengthy strings.
#'
#' @param x [\code{character}]
#'   Vector of strings.
#' @param split [\code{character(1)}]\cr
#'   Character string used to split each component of \code{x}.
#' @param nchars [\code{integer(1)}]\cr
#'   Length of extracted prefix for each part.
#'   Defaults to 1.
#' @param sep [\code{string}]\cr
#'   Possibly empty glue string.
#'   Default is \dQuote{-}.
#' @param ... [\code{any}]\cr
#'   Further arguments passed down to \code{\link[base]{strsplit}}.
#'   The most important here is \code{split}.
#' @return [\code{character}] Shortcut version of \code{x}.
#' @examples
#' x = c("hello-world", "some-length-string")
#' str_to_shortcut(x, split = "-")
#' str_to_shortcut(x, nchars = 2L, sep = "", split = "-")
#' @export
str_to_shortcut = function(x, split, nchars = 1, sep = "-", ...) {
  checkmate::assertCharacter(x, any.missing = FALSE, all.missing = FALSE)
  checkmate::assertString(split, min.chars = 1L)
  nchars = checkmate::asInt(nchars, lower = 1L)

  # split in parts
  exploded = strsplit(x, split = split, ...)

  # apply shortcut rules
  res = lapply(exploded, function(part) {
    sapply(part, substr, start = 1, stop = nchars)
  })

  # glue
  res = sapply(res, collapse, sep = sep)

  return(res)
}
