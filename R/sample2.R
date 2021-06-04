#' @title
#' Modified version of \code{\link[base]{sample}}.
#'
#' @description
#' Function \code{\link[base]{sample}}, given the first parameter
#' \code{x} is a single integer value, samples from the set \eqn{1,\ldots,x}.
#' This behavior might be undesirable when \code{\link[base]{sample}} is embedded
#' into routines and \code{x} is a variable.
#' This function remedies this issue. If \code{x} is a number it returns \code{x}
#' \code{size}-times.
#'
#' @inheritParams base::sample
#' @param ... [any]\cr
#'   Further parameters passed down to \code{\link[base]{sample}}, e.g. \code{prob}.
#' @return A vector.
#' @export
sample2 = function(x, size, replace = FALSE, ...) {
  n = length(x)
  if ((n == 1L) & (size > 1L) & !replace)
    re::stopf("Cannot sample %i elements from a set of cardinality 1 if replace=FALSE", size)
  if (n == 1L)
    return(rep(x, size))
  return(base::sample(x, size = size, replace = replace, ...))
}
