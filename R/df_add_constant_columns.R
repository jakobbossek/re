#' Add columns with constant values
#'
#' @param x [\code{data.frame}]\cr
#'   Data frame with at least one row and column.
#' @param ccols [\code{list}]\cr
#'   Named list of single elements.
#' @return Modified data frame.
#' @template family_dataframe_helpers
#' @export
#' @examples
#' x = data.frame(a = 1:2, b = 3:4)
#' y = df_add_constant_columns(x, list(c = "a", d = 0.1))
df_add_constant_columns = function(x, ccols) {
  checkmate::assert_data_frame(x, min.cols = 1, min.rows = 1L)
  checkmate::assert_list(ccols, min.len = 1L, names = "unique")
  cns = colnames(x)
  ncns = names(ccols)
  int = intersect(cns, ncns)
  if (length(int) > 0L)
    stopf("[df_add_constant_columns] Column(s) '%s' already exist in data frame x.", collapse(int))
  for (n in ncns) {
    x[[n]] = ccols[[n]]
  }
  return(x)
}
