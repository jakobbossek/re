#' @title Where is is min or max value in a nmeric vector?
#'
#' @param x [\code{numeric}]\cr
#'   Numeric vector.
#' @param return.method [\code{string}]\cr
#'   What to return in the presence of ties?
#'   \itemize{
#'   \item{\dQuote{first}: first occurence}
#'   \item{\dQuote{last}: last occurence}
#'   \item{\dQuote{random}: sampled uniformly at random from all positions}
#'   \item{\dQuote{all}: returns a vector of all positions}
#'   }
#'   Default is \dQuote{first}.
#' @return [\code{numeric}] Vector of integer position(s).
#' x = c(10, 24, 2, 2, 15, 2, 28)
#' lapply(c("first", "last", "random", "all"), function(m) {
#'  which.min(x, return.method = m)
#' })
#' @rdname which.min
#' @name which.min
#' @export
which.min = function(x, return.method = "first") {
  which.order(x, return.method, base::min)
}

#' @rdname which.min
#' @export
which.max = function(x, return.method = "first") {
  which.order(x, return.method, base::max)
}

which.order = function(x, return.method = "first", fun) {
  checkmate::assert_numeric(x, min.len = 1L)
  checkmate::assert_choice(return.method, choices = c("first", "last", "random", "all"))
  idx = which(x == fun(x))
  switch(
    return.method,
    "first"  = idx[1L],
    "last"   = idx[length(idx)],
    "random" = sample2(idx, size = 1L),
    "all"    = idx)
}
