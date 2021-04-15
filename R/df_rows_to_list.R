#' @title Convert data frame rows to list.
#'
#' @description Returns a list of lists with each sub-list being a
#' row of the data frame.
#'
#' @param x [\code{data.frame}]\cr
#'   Data frame.
#' @param named [\code{logical(1)}]\cr
#'   Should the sub-lists be named?
#'   If \code{TRUE} the \code{colnames(x)} are used if available.
#'   Otherwise names \dQuote{V1, ...,VX} are used where X equals
#'   \code{ncol(x)}.
#'   Default is \code{TRUE}.
#' @return [\code{list}] List of (named) lists.
#' @examples
#' x = data.frame(x = 1:3, y = letters[3:5])
#' df_rows_to_list(x)
#' df_rows_to_list(x, named = FALSE)
#' df_rows_to_list(unname(x), named = FALSE)
#' @export
df_rows_to_list = function(x, named = TRUE) {
  checkmate::assertDataFrame(x, min.rows = 1L)
  ns = colnames(x)
  is.named = !is.null(ns)
  nc = ncol(x)
  lapply(seq_len(nrow(x)), function(i) {
    l = unname(as.list(x[i, ]))
    attributes(l) = NULL
    if (named) {
      if (is.named)
        names(l) = ns
      else
        names(l) = paste0("X", 1:nc)
    }
    return(l)
  })
}
