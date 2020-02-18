#' Add columns with constant values
#'
#' @param x [\code{data.frame}]\cr
#'   Data frame with at least one row and column.
#' @param ccols [\code{list}]\cr
#'   Named list of single elements.
#' @return [\code{data.frame}]
#' @examples
#' x = data.frame(a = 1:2, b = 3:4)
#' y = addConstantColumns(x, list(c = "a", d = 0.1))
#' @export
addConstantColumns = function(x, ccols) {
  checkmate::assertDataFrame(x, min.cols = 1, min.rows = 1L)
  checkmate::assertList(ccols, min.len = 1L, names = "unique")
  cns = colnames(x)
  ncns = names(ccols)
  int = intersect(cns, ncns)
  if (length(int) > 0L)
    stopf("[addConstantColumns] Column(s) '%s' already exist in data frame x.", collapse(int))
  for (n in ncns) {
    x[[n]] = ccols[[n]]
  }
  return(x)
}
