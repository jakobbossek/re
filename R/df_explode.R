#' @title Split variable (column) into multiple.
#'
#' @description Split/explode a data frame character column into multiple columns.
#' This is done by splitting the string by one or multiple delimiters. The split
#' results are wrapped in a \code{data.frame} and \dQuote{cbinded} to \code{x}.
#' Optionally, one can pass column names and/or types for the parts. E.g.
#' \code{types="cir"} converts the first part into character, the second into
#' integer and the third into real/double.
#'
#' @param x [\code{data.frame}]\cr
#'   Data frame.
#' @param split.col [\code{character(1)}]\cr
#'   Character name of column used for splitting.
#' @param split [\code{character(1)}]\cr
#'   See parameter \code{split} of \code{\link[base]{strsplit}}.
#' @param names [\code{character}]\cr
#'   Optional column names.
#'   If \code{NULL}, the column names are V1 to VN where N is the number of
#'   split parts.
#' @param types [\code{character(1)}]\cr
#'   Character string where the i-th character specifies the atomic data type of
#'   the i-th split part. Possible characters are \dQuote{'c'} (character),
#'   \dQuote{'f'} (factor), dQuote{'i'} (integer), \dQuote{'l'} (logical) or
#'   \dQuote{n}, \dQuote{d} or \dQuote{r} for numeric/double/real.
#'   Default is \code{NULL}. In this case all split parts are characters.
#' @param keep [\code{logical(1)}]\cr
#'   Single logical value indicating whether column \code{split.col} should
#'   be kept or dropped.
#'   Default is \code{FALSE}.
#' @return [\code{data.frame}]
#' @export
#' @examples
#' x = data.frame(a = c("x-y-1", "a-b-2", "c-d-3"), b = 1:3, c = runif(3))
#' df_explode(x, split.col = "a", split = "-")
#' df_explode(x, split.col = "a", split = "-", keep = TRUE)
#' df_explode(x, split.col = "a", split = "-", names = c("A", "B", "C"))
#' df_explode(x, split.col = "a", split = "-", types = "cci")
df_explode = function(x, split.col, split, names = NULL, types = NULL, keep = FALSE) {
  checkmate::assert_data_frame(x, min.rows = 1L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_flag(keep)
  if (!(split.col %in% colnames(x)))
    re::stopf("[df_explode] Argument split.col needs to be a column name of
      data frame x.")

  if (any(names %in% colnames(x)))
    re::stopf("[df_explode] Some names given in argument \"names\" are already
      used in as column names in x.")

  expl = str_explode(as.character(x[[split.col]]), split, names, types)
  if (!keep)
    x[[split.col]] = NULL

  cbind(expl, x, stringsAsFactors = FALSE)
}
