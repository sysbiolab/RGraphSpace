
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
#' data('gtoy1', package = 'RGraphSpace')
#'
#' gs <- GraphSpace(gtoy1)
#' 
#' @importFrom igraph degree vcount ecount which_mutual
#' @importFrom igraph as_edgelist as_adjacency_matrix is_simple 
#' @importFrom igraph simplify V E 'V<-' 'E<-' is_directed vertex_attr
#' @importFrom igraph layout_nicely as.undirected delete_edge_attr
#' @importFrom igraph vertex_attr_names edge_attr edge_attr_names
#' @importFrom igraph ends delete_vertex_attr 'edge_attr<-'
#' @importFrom scales rescale
#' @aliases GraphSpace
#' @export
#'
GraphSpace <- function(g, layout = NULL, mar = 0.075, verbose = TRUE) {
    if(!is.null(layout)) .validate.args("numeric_mtx", "layout", layout)
    .validate.args("singleNumber", "mar", mar)
    .validate.args("singleLogical", "verbose", verbose)
    #--- validate argument values
    if (mar < 0 || mar > 1) {
        stop("'mar' should be in [0,1]", call. = FALSE)
    }
    #--- validate the igraph object
    g <- .validate.igraph(g, layout, verbose)
    gs <- .buildGraphSpace(g, mar, verbose)
    return(gs)
}

#-------------------------------------------------------------------------------
#' @title Plotting igraph objects with RGraphSpace package
#'
#' @description \code{plotGraphSpace} is a wrapper function to 
#' create dedicated ggplot graphics for igraph- and GraphSpace-class objects.
#'
#' @param gs Either an \code{igraph} or \linkS4class{GraphSpace} class object.
#' If \code{gs} is an \code{igraph}, then it must include \code{x}, \code{y}, 
#' and \code{name}  vertex attributes (see \code{\link{GraphSpace}}).
#' @param xlab The title for the 'x' axis of a 2D-image space.
#' @param ylab The title for the 'y' axis of a 2D-image space.
#' @param font.size A single numeric value passed to ggplot themes.
#' @param theme Name of a custom RGraphSpace theme. These themes 
#' (from 'th1' to 'th3') consist mainly of preconfigured ggplot settings, 
#' which the user can subsequently fine-tune within the resulting 
#' ggplot object.
#' @param bg.color A single color for background.
#' @param marks A logical value indicating whether to add 'marks' to vertex 
#' positions. Alternatively, this could be a vector listing vertex names.
#' @param mark.size A font size argument passed to \code{\link[ggplot2]{geom_text}}.
#' @param mark.color A color passed to \code{\link[ggplot2]{geom_text}}.
#' @return A ggplot-class object.
#' @author Sysbiolab.
#' @seealso \code{\link{GraphSpace}}
#' @examples
#' # Load a demo igraph
#' data('gtoy1', package = 'RGraphSpace')
#' 
#' # Generate a ggplot for gtoy1
#' plotGraphSpace(gtoy1)
#' 
#' # Create a GraphSpace object
#' gs <- GraphSpace(gtoy1)
#' 
#' # Generate a ggplot for gs
#' plotGraphSpace(gs)
#' 
#' @import methods
#' @importFrom ggplot2 geom_point geom_segment aes Geom .pt
#' @importFrom ggplot2 element_rect margin element_blank layer
#' @importFrom ggplot2 element_line element_text ggproto
#' @importFrom ggplot2 scale_linetype_manual
#' @importFrom grDevices col2rgb
#' @importFrom grid gpar arrow unit pointsGrob
#' @importFrom scales alpha
#' @docType methods
#' @rdname plotGraphSpace-methods
#' @aliases plotGraphSpace
#' @export
#'
setMethod("plotGraphSpace", "GraphSpace", 
    function(gs, xlab = "Graph coordinates 1", ylab = "Graph coordinates 2",
        font.size = 1, theme = c("th1", "th2", "th3"),
        bg.color = "grey95", marks = FALSE, mark.size = 3, 
        mark.color = "grey20") {
        #--- validate the gs object and args
        .validate.args("singleString", "xlab", xlab)
        .validate.args("singleString", "ylab", ylab)
        .validate.args("singleNumber", "font.size", font.size)
        theme <- match.arg(theme)
        .validate.colors("singleColor", "bg.color", bg.color)
        .validate.plot.args("marks", marks)
        .validate.args("numeric_vec","mark.size", mark.size)
        .validate.colors("singleColor","mark.color", mark.color)
        #--- get slots from gs
        nodes <- getGraphSpace(gs, "nodes")
        edges <- getGraphSpace(gs, "edges")
        pars <- getGraphSpace(gs, "pars")
        #--- get edge coordinates
        edges <- .get.exy(nodes, edges)
        #--- nodeSize is a '%' of plot space
        nodes$nodeSize <- nodes$nodeSize/100
        #--- set theme pars
        cl <- .set.theme.bks(theme)
        #--- get ggplot object
        ggp <- .set.gspace(nodes, xlab, ylab, cl)
        if(nrow(nodes)>0){
            ggp <- .add.graph(ggp, nodes, edges)
            #--- add marks if available
            bl1 <- (is.logical(marks) && marks) 
            bl2 <- (is.character(marks) && sum(marks%in%rownames(nodes))>0)
            if(bl1 || bl2){
                ggp <- .add.node.marks(ggp, nodes, marks,
                    mark.color, mark.size)
            }
        }
        #--- apply custom theme
        ggp <- .custom.themes(ggp, theme,
            font.size=font.size, bg.color=bg.color)
        return(ggp)
    }
)

#-------------------------------------------------------------------------------
#' @param ... Additional arguments passed to the 
#' \code{\link{plotGraphSpace}} function.
#' @param layout an optional numeric matrix with two columns for \code{x} 
#' and \code{y} coordinates.
#' @param mar A single numeric value (in \code{[0,1]}) indicating the size of
#' the outer margins as a fraction of the graph space.
#' @import methods
#' @docType methods
#' @rdname plotGraphSpace-methods
#' @aliases plotGraphSpace
#' @export
#'
setMethod("plotGraphSpace", "igraph", 
    function(gs, ..., layout = NULL, mar = 0.075) {
        gs <- GraphSpace(gs, layout, mar, verbose=FALSE)
        gg <- plotGraphSpace(gs, ...=...)
        return(gg)
    }
)

#-------------------------------------------------------------------------------
#' @title Accessors for fetching slots from a GraphSpace object
#'
#' @description \code{getGraphSpace} retrives information from
#' individual slots available in a GraphSpace object.
#'
#' @param gs A preprocessed \linkS4class{GraphSpace} class object
#' @param what A single character value specifying which information should 
#' be retrieved from the slots.
#' Options: 'graph','gxy','gxyz','pars','misc','status','summits',
#' 'summit_mask', and 'summit_contour'.
#' @return Content from slots in the \linkS4class{GraphSpace} object.
#' @examples
#' # Load a demo igraph
#' data('gtoy1', package = 'RGraphSpace')
#'
#' # Create a new GraphSpace object
#' gs <- GraphSpace(gtoy1)
#'
#' # Get the 'summary' slot in gs
#' getGraphSpace(gs, what = 'summary')
#'
#' @import methods
#' @docType methods
#' @rdname getGraphSpace-methods
#' @aliases getGraphSpace
#' @export
setMethod("getGraphSpace", "GraphSpace", function(gs, what = "summary") {
    opts <- c("nodes", "edges", "graph","pars", "misc", "summary")
    if (!what %in% opts) {
        opts <- paste0(opts, collapse = ", ")
        stop("'what' must be one of:\n", opts, call. = FALSE)
    }
    if (what == "nodes") {
        obj <- gs@nodes
    } else if (what == "edges") {
        obj <- gs@edges
    } else if (what == "graph") {
        obj <- gs@graph
    } else if (what == "pars") {
        obj <- gs@pars
    } else if (what == "misc") {
        obj <- gs@misc
    } else {
        obj <- summary(gs@graph)
    }
    return(obj)
})

#-------------------------------------------------------------------------------
# show summary information on screen
setMethod("show", "GraphSpace", function(object) {
    message("A GraphSpace-class object for:")
    getGraphSpace(object)
})
