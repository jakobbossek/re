#' @title Drop file extension(s).
#'
#' @param path [\code{character}]\cr
#'   Paths.
#' @param pos [\code{character(1)} | \code{integer(1)}]\cr
#'   This parameter specifies which dot in the filename should be considered to
#'   split the actual name from the file extension. Defaults to \dQuote{first},
#'   i.e., the first dot is used. Option \dQuote{last} anologously uses the last
#'   dot. Alternatively, one can pass a single integer specifying the position.
#' @return [\code{character}]
#' @export
fs_drop_ext = function(path, pos = "first") {
  if (!is.numeric(pos))
    checkmate::assert_choice(pos, choices = c("first", "last"))
  else
    pos = checkmate::asInt(pos, lower = 1L)
  dn = dirname2(path)
  bn = basename(path)
  dot_pos = unlist(gregexpr(pattern = '\\.', bn))
  n_dots = length(dot_pos)
  if (is.numeric(pos) && n_dots < pos)
    re::stopf("[fs_drop_ext] There are only %i dots in '%s', but pos(ition) %i passed.", n_dots, path, pos)
  if (n_dots == 0L)
    return(path)
  if (pos == "first")
    dot_pos = dot_pos[1L]
  else if (pos == "last")
    dot_pos = dot_pos[n_dots]
  else
    dot_pos = dot_pos[pos]
  file.path(dn, substr(bn, 1L, dot_pos - 1L))
}
