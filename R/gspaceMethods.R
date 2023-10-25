
#-------------------------------------------------------------------------------
#' @title Constructor of GraphSpace-class objects
#'
#' @description \code{GraphSpace} is a constructor of
#' GraphSpace-class objects.
#'
#' @param g An \code{igraph} object. It must include coordinates and names
#' assigned to \code{x}, \code{y}, and \code{name}  vertex attributes.
#' @param layout an optional numeric matrix with two columns for \code{x} 
#' and \code{y} coordinates.
#' @param mar A single numeric value (in \code{[0,1]}) indicating the size of
#' the outer margins as a fraction of the graph space.
#' @param verbose A single logical value specifying to display detailed 
#' messages (when \code{verbose=TRUE}) or not (when \code{verbose=FALSE}).
#' @return A \linkS4class{GraphSpace} class object.
#' @author Sysbiolab.
#' @seealso \code{\link{plotGraphSpace}}
#' @examples
#' # Load a demo igraph
#' data('gtoy1', package = 'GraphSpace')
#'
#' grs <- GraphSpace(gtoy1)
#' 
#' @importFrom igraph degree vcount ecount which_mutual
#' @importFrom igraph as_edgelist as_adjacency_matrix is.simple 
#' @importFrom igraph simplify V E 'V<-' 'E<-' is.directed vertex_attr
#' @importFrom igraph layout_nicely as.undirected delete_edge_attr
#' @importFrom igraph vertex_attr_names edge_attr edge_attr_names
#' @importFrom igraph delete_vertex_attr 'edge_attr<-'
#' @importFrom scales rescale
#' @aliases GraphSpace
#' @export
#'
GraphSpace <- function(g, layout = NULL, mar = 0.075, 
    verbose = TRUE) {
    if(!is.null(layout)) .validate.args("numeric_mtx", "layout", layout)
    .validate.args("singleNumber", "mar", mar)
    .validate.args("singleLogical", "verbose", verbose)
    #--- validate argument values
    if (mar < 0 || mar > 1) {
        stop("'mar' should be in [0,1]", call. = FALSE)
    }
    #--- validate the igraph object
    if(verbose) message("Validating the 'igraph' object...")
    g <- .validate.igraph(g, layout)
    grs <- .buildGraphSpace(g, mar, verbose)
    return(grs)
}

#-------------------------------------------------------------------------------
#' @title Accessors for fetching slots from a GraphSpace object
#'
#' @description \code{getGraphSpace} retrives information from
#' individual slots available in a GraphSpace object.
#'
#' @param grs A preprocessed \linkS4class{GraphSpace} class object
#' @param what A single character value specifying which information should 
#' be retrieved from the slots.
#' Options: 'graph','gxy','gxyz','pars','misc','status','summits',
#' 'summit_mask', and 'summit_contour'.
#' @return Content from slots in the \linkS4class{GraphSpace} object.
#' @examples
#' # Load a demo igraph
#' data('gtoy1', package = 'GraphSpace')
#'
#' # Create a new GraphSpace object
#' grs <- GraphSpace(gtoy1)
#'
#' # Get the 'summary' slot in grs
#' getGraphSpace(grs, what = 'summary')
#'
#' @import methods
#' @docType methods
#' @rdname getGraphSpace-methods
#' @aliases getGraphSpace
#' @export
setMethod("getGraphSpace", "GraphSpace", function(grs, what = "summary") {
    opts <- c("nodes", "edges", "graph","pars", "misc", "summary")
    if (!what %in% opts) {
        opts <- paste0(opts, collapse = ", ")
        stop("'what' must be one of:\n", opts, call. = FALSE)
    }
    if (what == "nodes") {
        obj <- grs@nodes
    } else if (what == "edges") {
        obj <- grs@edges
    } else if (what == "graph") {
        obj <- grs@graph
    } else if (what == "pars") {
        obj <- grs@pars
    } else if (what == "misc") {
        obj <- grs@misc
    } else {
        obj <- summary(grs@graph)
    }
    return(obj)
})

#-------------------------------------------------------------------------------
# show summary information on screen
setMethod("show", "GraphSpace", function(object) {
    message("A GraphSpace-class object for:")
    getGraphSpace(object)
})
