#' @title
#' Drop file extension(s)
#'
#' @param path [\code{character}]\cr
#'   Paths.
#' @param pos [\code{character(1)} | \code{integer(1)}]\cr
#'   This parameter specifies which dot in the filename should be considered to
#'   split the actual name from the file extension. Defaults to \dQuote{first},
#'   i.e., the first dot is used. Option \dQuote{last} anologously uses the last
#'   dot. Alternatively, one can pass a single integer specifying the position.
#' @param ext [\code{character(1)}]\cr
#'   Optional explicit character vector of file extension(s) to drop without
#'   leading dot.
#'   Default is \code{NULL}.
#' @return A character vector.
#' @template family_filesystem_helpers
#' @export
fs_drop_ext = function(path, pos = "first", ext = NULL) {
  checkmate::assert_character(ext, min.len = 1L, min.chars = 1L, any.missing = FALSE, all.missing = FALSE, null.ok = TRUE)
  if (!is.numeric(pos))
    checkmate::assert_choice(pos, choices = c("first", "last"))
  else
    pos = checkmate::asInt(pos, lower = 1L)
  dn = fs_dirname(path)
  bn = basename(path)

  # if explicit file extension is passed, drop these and return
  if (!is.null(ext)) {
    pattern = paste0("(", collapse(paste0("\\.", ext), sep = "|"), ")")
    return(file.path(dn, gsub(pattern, "", bn)))
  }

  dot_pos = gregexpr(pattern = '\\.', bn)
  dot_pos = sapply(dot_pos, function(dp) {
    n_dots = length(dp)
    if (is.numeric(pos) && n_dots < pos)
      re::stopf("[fs_drop_ext] There are only %i dots in '%s', but pos(ition) %i passed.", n_dots, path, pos)
    if (n_dots == 0L || dp[1L] == -1)
      dp = NA_integer_
    if (pos == "first")
      dp = dp[1L]
    else if (pos == "last")
      dp = dp[n_dots]
    else
      dp = dp[pos]
    return(dp)
  })

  # workaround for files where no dot was found in the basename
  idx.na = which(is.na(dot_pos))
  if (length(idx.na) > 0L) {
    dot_pos[idx.na] = sapply(bn[idx.na], nchar) + 1L
  }

  file.path(dn, substr(bn, 1L, dot_pos - 1L))
}
