setOldClass("igraph")
setClassUnion("array_Or_matrix", members = c("array", "matrix"))

#' @title GraphSpace: An S4 class for igraph objects
#'
#' @slot nodes A data frame with xy-vertex coordinates.
#' @slot edges  A data frame with edges.
#' @slot graph An igraph object.
#' @slot image An image matrix or array object.
#' @slot pars A list with parameters.
#' @slot misc A list with intermediate objects for downstream methods.
#'
#' @method plotGraphSpace \code{\link{plotGraphSpace}}
#' @method getGraphSpace \code{\link{getGraphSpace}}
#' @aliases GraphSpace-class
#' @return An S4 class object.
#' @section Constructor:
#' see \code{\link{GraphSpace}} constructor.
#' @import igraph
#' @exportClass GraphSpace
#'
## Class GraphSpace
setClass("GraphSpace",
    slot = c(
        nodes = "data.frame",
        edges = "data.frame",
        graph = "igraph",
        image = "array_Or_matrix",
        pars = "list",
        misc = "list"
    ),
    prototype = list(
        nodes = data.frame(),
        edges = data.frame(),
        graph = igraph::empty_graph(),
        image = array(),
        pars = list(),
        misc = list()
    )
)
