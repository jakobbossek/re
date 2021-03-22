#' @title Export ggplot object to file.
#'
#' @description Simple wrapper to \code{\link[ggplot2]{ggsave}} setting \code{limitsize=FALSE}
#' and \code{device=cairo_pdf} by default.
#'
#' @param filename [\code{character(1)}]\cr
#'   Path to file.
#' @param plot [\code{ggplot}]\cr
#'   ggplot object. Defaults to last plot.
#' @param ... [any]\cr
#'   Further parameters passed down to \code{\link[ggplot2]{ggsave}}.
#' @return Nothing
#' @export
ggsave2 = function(filename, plot = ggplot2::last_plot(), ...) {
  args = list(filename = filename, plot = plot, limitsize = FALSE, device = grDevices::cairo_pdf)
  args = re::insert(args, list(...))
  do.call(ggplot2::ggsave, args)
}
