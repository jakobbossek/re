#' @title Random shuffle of a vector.
#'
#' @description Basically an alias for \code{\link[base]{sample}}
#' Note that if the \code{x} is a scalar value the function returns
#' simply the value itself, i.e. acts like the identity function.
#'
#' @param x [\code{vector}]\cr
#'   Input vector.
#' @return [\code{vector}] Shuffled/scrambled \code{x}.
#' @expamples
#' shuffle(1:10)
#' @export
shuffle = function(x) {
  sample2(x, size = length(x))
}
