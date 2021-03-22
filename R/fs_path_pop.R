#' @title Stack-like pop operation for path names.
#'
#' @param path [\code{character}]\cr
#'   Paths.
#' @param depth [\code{integer(1)}]\cr
#'   How many parts should be dropped?
#'   Default to 1. In this case the function behaves like \code{basename(path)}.
#' @return [\code{character}]
#' @export
fs_path_pop = function(path, depth = 1L) {
  depth = checkmate::asInt(depth, lower = 1L)
  exploded = base::strsplit(path, split = .Platform$file.sep)
  sapply(exploded, function(e) {
    n = length(e)
    if (n < depth)
      return(NA_character_)
    re::collapse(e[(n-depth+1):n], sep = .Platform$file.sep)
  })
}
