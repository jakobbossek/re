#' @title Get number of unique elements.
#'
#' @description Given a vector of atomic type (\code{logical}, \code{integer},
#' \code{numeric}, \code{complex}, \code{character}) or a factor the function
#' returns the number of different elements.
#'
#' @note NAs are counted as a seperate value.
#'
#' @param x [\code{atomic} | \code{factor}]\cr
#'   A vector.
#' @return [\code{integer(1)}]
#' @examples
#' x = c(1, 2, 2, 3, 2, NA, NA, 5)
#' nunique(x) # 5
#' @export
nunique = function(x) {
  if (!is.atomic(x) && !is.factor(x))
    stopf("[nunique] Not available for non-atomic/non-factor inputs.")
  sum(!duplicated(x))
}
