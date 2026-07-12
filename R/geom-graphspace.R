
#-------------------------------------------------------------------------------
#' @title Convenience wrapper for node and edge geoms
#'
#' @description
#' `geom_graphspace()` adds both node and edge layers to a \pkg{ggplot2}
#' plot by calling \code{\link{geom_nodespace}} and \code{\link{geom_edgespace}} 
#' in sequence. It is a convenience wrapper with no logic of its own; any 
#' argument accepted by either underlying geom can be passed via 
#' \code{node.params} or \code{edge.params}.
#'
#' For independent control of node and edge layers, use
#' \code{\link{geom_nodespace}} and \code{\link{geom_edgespace}} directly.
#'
#' @param mapping An optional \code{\link[ggplot2]{aes}} call passed to
#'   \code{\link{geom_nodespace}}. The most common use is supplying node
#'   label aesthetics, e.g. \code{aes(label = nodeLabel)}.
#' @param node.params A named list of additional arguments forwarded
#'   to \code{\link{geom_nodespace}}.
#' @param edge.params A named list of additional arguments forwarded
#'   to \code{\link{geom_edgespace}}.
#'
#' @return A list of two \pkg{ggplot2} layers, which \pkg{ggplot2}
#'   flattens automatically when added to a plot with `+`.
#'
#' @examples
#' library(ggplot2)
#' data("gtoy1", package = "RGraphSpace")
#' gs <- GraphSpace(gtoy1)
#'
#' # Simplest use
#' ggplot(gs) + geom_graphspace()
#'
#' # With node labels
#' ggplot(gs) + geom_graphspace(aes(label = nodeLabel))
#'
#' # With independent node and edge customization
#' ggplot(gs) + geom_graphspace(
#'   node.params = list(aes(label = nodeLabel)),
#'   edge.params = list(curve = 0.3)
#' )
#'
#' @seealso \code{\link{geom_nodespace}}, \code{\link{geom_edgespace}},
#' \code{\link{plotGraphSpace}}
#'
#' @export
geom_graphspace <- function(mapping = NULL,
  node.params = list(), edge.params = list()) {
  
  if (!is.null(mapping)) {
    node.params[["mapping"]] <- mapping
  }
  list(
    do.call(geom_edgespace, edge.params),
    do.call(geom_nodespace, node.params)
  )
  
}

