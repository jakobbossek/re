#' @title
#' Collapse string
#'
#' @param x [\code{vector}]\cr
#'   Input vector.
#' @param sep [\code{character(1)}]\cr
#'   Character used to seperate the entries of \code{x} in the produced string.
#' @return A character vector.
#' @export
collapse = function (x, sep = ",") {
  base::paste0(x, collapse = sep)
}
