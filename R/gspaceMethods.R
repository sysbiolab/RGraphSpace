
#-------------------------------------------------------------------------------
#' @title Constructor of GraphSpace-class objects
#'
#' @description \code{GraphSpace} is a constructor of
#' GraphSpace-class objects.
#'
#' @param g An \code{\link[igraph]{igraph}} object. It must include graph 
#' coordinates assigned to \code{x} and \code{y} vertex attributes, and
#' vertex labels assigned to \code{name} vertex attribute.
#' @param mar A single numeric value (in \code{[0,1]}) indicating the size of
#' the outer margins as a fraction of the graph space.
#' Note: When an image is provided, \code{mar} is a fraction of image margins.
#' @param layout An optional numeric matrix with two columns for \code{x} 
#' and \code{y} coordinates.
#' @param image An optional background image. When provided, \code{x} and 
#' \code{y} coordinates must represent pixel positions in the image space.
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
#' @importFrom igraph degree vcount ecount which_mutual any_loop any_multiple
#' @importFrom igraph as_edgelist as_adjacency_matrix is_simple 
#' @importFrom igraph graph_attr_names delete_graph_attr empty_graph
#' @importFrom igraph simplify V E 'V<-' 'E<-' is_directed vertex_attr
#' @importFrom igraph layout_nicely as_undirected delete_edge_attr
#' @importFrom igraph vertex_attr_names edge_attr edge_attr_names
#' @importFrom igraph ends delete_vertex_attr 'edge_attr<-'
#' @importFrom scales rescale
#' @importFrom grDevices is.raster as.raster
#' @aliases GraphSpace
#' @export
#'
GraphSpace <- function(g, mar = 0.1, layout = NULL, image = NULL, 
    verbose = TRUE) {
    .validate.args("singleNumber", "mar", mar)
    .validate.args("singleLogical", "verbose", verbose)
    #--- validate argument values
    if (mar < 0 || mar > 1) {
        stop("'mar' should be in [0,1]", call. = FALSE)
    }
    if(!is.null(layout)){
        .validate.args("numeric_mtx", "layout", layout)
        if (ncol(layout) != 2) {
            stop("'layout' matrix should have two columns.", call. = FALSE)
        } 
    }
    if(!is.null(image)){
        .validate.args("image_mtx", "image", image)
    }
    #--- validate igraph and build a gs object
    gs <- .buildGraphSpace(g, layout, mar, image, verbose)
    return(gs)
}

#-------------------------------------------------------------------------------
#' @title Plotting igraph objects with RGraphSpace
#'
#' @description \code{plotGraphSpace} is a wrapper function to 
#' create dedicated ggplot graphics for igraph- and GraphSpace-class objects.
#'
#' @param gs Either an \code{igraph} or \linkS4class{GraphSpace} class object.
#' If \code{gs} is an \code{igraph}, then it must include \code{x}, \code{y}, 
#' and \code{name}  vertex attributes (see \code{\link{GraphSpace}}).
#' @param theme Name of a custom RGraphSpace theme. These themes 
#' (from 'th1' to 'th3') consist of preconfigured ggplot settings, 
#' which the user can subsequently refine using \code{\link[ggplot2]{ggplot2}}.
#' @param xlab The title for the 'x' axis of a 2D-image space.
#' @param ylab The title for the 'y' axis of a 2D-image space.
#' @param font.size A single numeric value passed to ggplot themes.
#' @param bg.color A single color for background.
#' @param add.labels A logical value indicating whether to plot vertex labels.
#' @param node.labels A vector of vertex names to be highlighted in the graph
#' space. This argument overrides 'add.labels'.
#' @param label.size A size argument passed to \code{\link[ggplot2]{geom_text}}.
#' @param label.color A color passed to \code{\link[ggplot2]{geom_text}}.
#' @param add.image A logical value indicating whether to add a background 
#' image, when one is available (see \code{\link{GraphSpace}}).
#' @param marks Deprecated from RGraphSpace 1.0.9; use 'node.labels' instead.
#' @param mark.size Deprecated from RGraphSpace 1.0.9; use 'label.size' instead.
#' @param mark.color Deprecated from RGraphSpace 1.0.9; use 'label.color' instead.
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
#' @importFrom ggplot2 geom_point geom_segment aes Geom .pt geom_text
#' @importFrom ggplot2 element_rect margin element_blank layer theme_bw
#' @importFrom ggplot2 element_line element_text ggproto theme theme_gray
#' @importFrom ggplot2 scale_linetype_manual annotation_raster coord_fixed
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous expansion
#' @importFrom grDevices col2rgb
#' @importFrom grid gpar arrow unit pointsGrob
#' @importFrom scales alpha
#' @importFrom lifecycle deprecated deprecate_soft is_present
#' @docType methods
#' @rdname plotGraphSpace-methods
#' @aliases plotGraphSpace
#' @export
#'
setMethod("plotGraphSpace", "GraphSpace", 
    function(gs, theme = c("th0", "th1", "th2", "th3"),
        xlab = "Graph coordinates 1", ylab = "Graph coordinates 2", 
        font.size = 1, bg.color = "grey95", add.labels = FALSE,
        node.labels = NULL, label.size = 3, label.color = "grey20", 
        add.image = FALSE, marks = deprecated(), 
        mark.size = deprecated(), mark.color = deprecated()) {
        ### deprecate
        if (lifecycle::is_present(marks)) {
            deprecate_soft("1.0.9", "plotGraphSpace(marks)", 
                "plotGraphSpace(node.labels)")
            node.labels <- marks
        }
        if (lifecycle::is_present(mark.size)) {
            deprecate_soft("1.0.9", "plotGraphSpace(mark.size)", 
                "plotGraphSpace(label.size)")
            label.size <- mark.size
        }
        if (lifecycle::is_present(mark.color)) {
            deprecate_soft("1.0.9", "plotGraphSpace(mark.color)", 
                "plotGraphSpace(label.color)")
            label.color <- mark.color
        }
        ###
        #--- validate the gs object and args
        .validate.args("singleString", "xlab", xlab)
        .validate.args("singleString", "ylab", ylab)
        .validate.args("singleNumber", "font.size", font.size)
        .validate.colors("singleColor", "bg.color", bg.color)
        .validate.args("singleLogical", "add.labels", add.labels)
        .validate.args("singleNumber", "label.size", label.size)
        .validate.colors("singleColor", "label.color", label.color)
        .validate.args("singleLogical", "add.image", add.image)
        .validate.plot.args("node.labels", node.labels)
        theme <- match.arg(theme)
        
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
        
        #--- initialize a ggplot object
        ggp <- .set.gspace(nodes, xlab, ylab, cl)
        
        #--- add image
        if(pars$image.layer){
            img <- getGraphSpace(gs, "image")
            if(add.image){
                ggp <- .add.image(ggp, img)
            } else {
                ggi <- .add.image(ggp, img)
                ggi <- .custom.themes(ggi, theme, font.size, bg.color)
            }
        }

        #--- add nodes
        if(nrow(nodes)>0){
            ggp <- .add.graph(ggp, nodes, edges)
            #--- add node labels
            if (!is.null(node.labels)){
                ggp <- .add.labels1(ggp, nodes, node.labels, 
                    label.size, label.color)
            } else if(add.labels){
                ggp <- .add.labels2(ggp, nodes)
            }
        }
        
        #--- apply custom theme
        ggp <- .custom.themes(ggp, theme, font.size, bg.color)
        
        if(pars$image.layer && !add.image){
            ggl <- list(graph = ggp, image = ggi)
            return(ggl)
        } else {
            return(ggp)
        }
        
    }
)

#-------------------------------------------------------------------------------
#' @param ... Additional arguments passed to the 
#' \code{\link{plotGraphSpace}} function.
#' @param mar A single numeric value (in \code{[0,1]}) indicating the size of
#' the outer margins as a fraction of the graph space.
#' @import methods
#' @docType methods
#' @rdname plotGraphSpace-methods
#' @aliases plotGraphSpace
#' @export
#'
setMethod("plotGraphSpace", "igraph", 
    function(gs, ..., mar = 0.1) {
        gs <- GraphSpace(gs, mar, verbose=FALSE)
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
    opts <- c("nodes", "edges", "graph","pars", "misc", "summary", "image")
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
    } else if (what == "image") {
        obj <- gs@image
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
