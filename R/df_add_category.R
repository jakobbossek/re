#' Add category for column.
#'
#' @param x [\code{data.frame}]\cr
#'   Data frame with at least one row and column.
#' @param col [\code{integer} | \code{string}]\cr
#'   Column number or colmumn name.
#' @param values [\code{character}]\cr
#'   Which values of the column \code{col} shall be unioned into the new category.
#' @param new.value [\code{any}]\cr
#'   New category value.
#' @param factor.handling [\code{string}]\cr
#'   Determines how to handle the case that \code{col} is a factor column.
#'   Option \dQuote{drop} converts the factor into a character column. Option
#'   \dQuote{keep} does the same, but after adding the new category, the factor
#'   property is reestablished. If the factor was ordered, the new category is placed
#'   last.
#' @return [\code{data.frame}]
#' @export
df_add_category = function(x, col, values, new.value, factor.handling = "keep") {
  checkmate::assertDataFrame(x, min.cols = 1L, min.rows = 1L)
  checkmate::assertChoice(factor.handling, choices = c("drop", "keep"))
  nc = ncol(x)
  cns = colnames(x)
  if (!is.numeric(x) & !(col %in% cns))
    stopf("[df_add_category] Parameter col needs to be a valid column name of x or a column number.")

  col.was.factor = FALSE
  col.fac.levels = NA
  col.fac.ordered = NA

  if (is.factor(x[[col]])) {
    catf("[df_add_category] Converting factor column to character.")
    col.was.factor = TRUE
    col.levels = levels(x[[col]])
    col.ordered = is.ordered(x[[col]])
    x[[col]] = as.character(x[[col]])
  }
  col.values = x[[col]]

  idx.sel = if (missing(values)) {
    1:length(col.values)
  } else {
    which(col.values %in% values)
  }

  x2 = x[idx.sel, , drop = FALSE]
  x2[[col]] = new.value

  x = rbind(x, x2)

  # now repair factors: new category is last factor if it was ordered originally
  if (col.was.factor & (factor.handling == "keep")) {
    # append new value at the end
    col.levels = c(col.levels, new.value)
    x[[col]] = factor(x[[col]], levels = col.levels, ordered = col.ordered)
  }
  return(x)
}

