
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
    .validate_gs_args("singleNumber", "mar", mar)
    .validate_gs_args("singleLogical", "verbose", verbose)
    #--- validate argument values
    if (mar < 0 || mar > 1) {
        stop("'mar' should be in [0,1]", call. = FALSE)
    }
    if(!is.null(layout)){
        .validate_gs_args("numeric_mtx", "layout", layout)
        if (ncol(layout) != 2) {
            stop("'layout' matrix should have two columns.", call. = FALSE)
        } 
    }
    if(!is.null(image)){
        .validate_gs_args("image_mtx", "image", image)
    }
    #--- validate igraph and build a gs object
    gs <- .buildGraphSpace(g, mar, image, layout, verbose)
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
#' @param raster A logical value indicating whether to rasterize the main plot.
#' See \code{\link[ggrastr]{rasterise}} for further specifications.
#' @param dpi Raster resolution, in dots per inch.
#' @param dev Device used in the \code{\link[ggrastr]{rasterise}} call.
#' @return A ggplot-class object.
#' @author Sysbiolab.
#' @seealso \code{\link{GraphSpace}}
#' @examples
#' # Load a demo igraph
#' data('gtoy1', package = 'RGraphSpace')
#' 
#' # Generate a ggplot for igraph
#' plotGraphSpace(gtoy1)
#' 
#' # Create a GraphSpace object
#' gs <- GraphSpace(gtoy1)
#' 
#' # Generate a ggplot for gs
#' plotGraphSpace(gs)
#' 
#' @import methods
#' @importFrom grDevices col2rgb
#' @importFrom ggrastr rasterize
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
        add.image = FALSE, raster = FALSE, dpi = 300, dev = "cairo_png") {
        #--- validate the gs object and args
        .validate_gs_args("singleString", "xlab", xlab)
        .validate_gs_args("singleString", "ylab", ylab)
        .validate_gs_args("singleNumber", "font.size", font.size)
        .validate_gs_colors("singleColor", "bg.color", bg.color)
        .validate_gs_args("singleLogical", "add.labels", add.labels)
        .validate_gs_args("singleNumber", "label.size", label.size)
        .validate_gs_colors("singleColor", "label.color", label.color)
        .validate_gs_args("singleLogical", "add.image", add.image)
        .validate_gs_args("singleLogical", "raster", raster)
        .validate_gs_args("singleInteger", "dpi", dpi)
        .validate_gs_args("singleString", "dev", dev)
        if (!is.null(node.labels)) {
            .validate_gs_args("allCharacter", "node.labels", node.labels)
        }
        theme <- match.arg(theme)
        
        #--- get gs slots
        nodes <- gs_nodes(gs)
        edges <- gs_edges(gs)
        pars <- getGraphSpace(gs, "pars")
        
        #--- use only one color entry when shape < 21
        idx <- nodes$nodeShape < 21
        nodes$nodeLineColor[idx] <-  nodes$nodeColor[idx]
        
        #--- initialize a ggplot object
        ggp <- .set_gspace(theme)
        
        #--- add labels
        ggp <- ggp + labs(x=xlab, y=ylab)
        
        #--- add image
        if(pars$image.layer){
            img <- getGraphSpace(gs, "image")
            if(add.image){
                ggp <- .add_image(ggp, img)
            } else {
                ggi <- .add_image(ggp, img)
                ggi <- .custom_themes(ggi, theme, font.size, bg.color)
            }
        }

        #--- add edges
        if(nrow(edges)>0){
            x <- y <- xend <- yend <- NULL
            edgeLineType <- edgeLineWidth <- edgeLineColor <- NULL 
            ggp <- ggp + geom_edgespace(
                mapping = aes(x = x, y = y, xend = xend, yend = yend),
                linetype = edges$edgeLineType, linewidth = edges$edgeLineWidth,
                colour = edges$edgeLineColor,
                data = gs)
        }
        
        #--- add nodes
        if(nrow(nodes)>0){
            x <- y <- nodeColor <- nodeLineColor <- NULL
            nodeShape <- nodeLineWidth <- nodeSize <- NULL
            ggp <- ggp + geom_nodespace(mapping = aes(x = x, y = y), 
                fill = nodes$nodeColor, colour = nodes$nodeLineColor,
                shape = nodes$nodeShape, size = nodes$nodeSize, 
                linewidth = nodes$nodeLineWidth, 
                data = nodes)
            
            #--- add node labels
            if (!is.null(node.labels)){
                ggp <- .add_labels1(ggp, nodes, node.labels, 
                    label.size, label.color)
            } else if(add.labels){
                ggp <- .add_labels2(ggp, nodes)
            }
        }
        
        #--- apply custom theme
        ggp <- .custom_themes(ggp, theme, font.size, bg.color)
        
        if(raster){
          ggp <- ggrastr::rasterize(ggp, layers=c('NodeSpace',"EdgeSpace"), 
              dpi = dpi, dev = dev)
        }
        
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

#' Plot GraphSpace objects
#' 
#' @param x A \linkS4class{GraphSpace} class object.
#' @param ... Additional arguments passed to the 
#' \code{\link{plotGraphSpace}} function.
#' @seealso \code{\link{plotGraphSpace}}
#' 
#' @importFrom graphics plot
#' @export
#'
plot.GraphSpace <- function(x, ...) {
    plotGraphSpace(x, ...)
}

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
#' getGraphSpace(gs, what = 'graph')
#'
#' @import methods
#' @docType methods
#' @rdname getGraphSpace-methods
#' @aliases getGraphSpace
#' @export
setMethod("getGraphSpace", "GraphSpace", function(gs, what = "graph") {
    opts <- c("nodes", "edges", "graph","pars", "misc", "image")
    if (!what %in% opts) {
        opts <- paste0(opts, collapse = ", ")
        stop("'what' must be one of:\n", opts, call. = FALSE)
    }
    if (what == "nodes") {
        obj <- gs@nodes
    } else if (what == "edges") {
        obj <- .get_edge_coords(gs)
    } else if (what == "graph") {
        obj <- gs@graph
    } else if (what == "pars") {
        obj <- gs@pars
    } else if (what == "misc") {
        obj <- gs@misc
    } else if (what == "image") {
        obj <- gs@image
    } else {
        obj <- gs@graph
    }
    return(obj)
})

#-------------------------------------------------------------------------------
# show summary information on screen
setMethod("show", "GraphSpace", function(object) {
    message("A GraphSpace-class object for:")
    obj <- getGraphSpace(object, what = "graph")
    summary(obj)
})

#-------------------------------------------------------------------------------
#' @title Accessors for applying essential igraph methods to modify
#' attributes of GraphSpace objects.
#' 
#' @description Access and modify individual slots of a GraphSpace 
#' object. Selected 'igraph' methods are applied to the 'graph' slot and 
#' propagated to downstream components.
#' 
#' @param x A \linkS4class{GraphSpace} class object
#' @param name Name of the attribute.
#' @param value The new value of the attribute.
#' @param ... Additional arguments passed to igraph methods.
#' @return Updated \linkS4class{GraphSpace} object.
#' @seealso \code{\link[igraph]{vertex_attr}}, \code{\link[igraph]{edge_attr}}
#' @examples
#' # Load a demo igraph
#' data('gtoy1', package = 'RGraphSpace')
#' 
#' # Create a new GraphSpace object
#' gs <- GraphSpace(gtoy1)
#' 
#' #--- Usage of GraphSpace attribute accessors:
#'
#' # Get a data frame with nodes for plotting methods
#' gs_nodes(gs)
#' 
#' # Get a data frame with edges for plotting methods
#' gs_edges(gs)
#' 
#' # Get vertex count
#' gs_vcount(gs)
#' 
#' # Get edge count
#' gs_ecount(gs)
#' 
#' # Get vertex names
#' names(gs)
#' 
#' # Access all vertex attributes
#' gs_vertex_attr(gs)
#' 
#' # Access a specific vertex attribute
#' gs_vertex_attr(gs, "nodeLabel")
#' 
#' # Modify a single value within a vertex attribute
#' gs_vertex_attr(gs, "nodeSize")["n1"] <- 10
#' 
#' # Replace an entire vertex attribute
#' gs_vertex_attr(gs, "nodeSize") <- 10
#' 
#' # Alternative syntax using `$` accessor
#' gs_vertex_attr(gs)$nodeSize <- 10
#' 
#' # Access a specific edge attribute
#' gs_edge_attr(gs, "edgeLineColor")
#' 
#' # Replace an entire edge attribute
#' gs_edge_attr(gs, "edgeLineWidth") <- 1
#' 
#'  # Alternative syntax using `$` for edge attributes
#' gs_edge_attr(gs)$edgeLineWidth <- 3
#' 
#' @aliases names
#' @aliases names<-
#' @aliases gs_nodes
#' @aliases gs_edges
#' @aliases gs_vcount
#' @aliases gs_ecount
#' @aliases gs_vertex_attr
#' @aliases gs_edge_attr
#' @aliases gs_vertex_attr<-
#' @aliases gs_edge_attr<-
#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_nodes", "GraphSpace", function(x) {
    nodes <- .get_node_coords(x)
    return(nodes)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_edges", "GraphSpace", function(x) {
    edges <- .get_edge_coords(x)
    return(edges)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_vcount", "GraphSpace", function(x) {
    igraph::vcount(x@graph)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_ecount", "GraphSpace", function(x) {
    igraph::ecount(x@graph)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("names", "GraphSpace", function(x) {
    x@nodes$name
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("names<-", "GraphSpace", function(x, value) {
    gs_vertex_attr(x, "name") <- value
    return(x)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_vertex_attr", "GraphSpace", function(x, name, ...) {
    g <- x@graph
    if(missing(name)){
        att <- igraph::vertex_attr(graph = g, ...=...)
    } else {
        if(name %in% igraph::vertex_attr_names(g)){
            att <- igraph::vertex_attr(graph = g, name = name, ...=...)
            if(name!="name") names(att) <- V(g)$name
        } else {
            att <- NULL
        }
    }
    return(att)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_vertex_attr<-", "GraphSpace", function(x, name, ..., value) {
    g <- x@graph
    if (missing(name) && is.list(value)){
        #workaround for analogy to igraph's "syntactic sugar" $<-
        len1 <- unlist(lapply(value, length))==1
        if(any(len1)){
            for(i in which(len1)){
                vl <- value[[i]]
                vl <- ifelse(.is_replicable(vl), vl, list(vl))
                value[[i]] <- rep(vl, igraph::vcount(g))
            }
        }
        igraph::vertex_attr(graph = g) <- value
    } else {
        if(length(value)==1){
            value <- ifelse(.is_replicable(value), value, list(value))
        }
        igraph::vertex_attr(graph = g, name = name, ...=...) <- value  
    }
    x <- .updateGraphSpace(x, g)
    return(x)
})
.is_replicable <- function(x) {
    tryCatch({
        rep(x, 2)
        TRUE
    }, error = function(e) FALSE)
}

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_edge_attr", "GraphSpace", function(x, name, ...) {
    g <- x@graph
    att <- igraph::edge_attr(graph = g, name = name, ...=...)
    return(att)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_edge_attr<-", "GraphSpace", function(x, name, ..., value) {
    g <- x@graph
    if (missing(name) && is.list(value)){
        #workaround for analogy to igraph's "syntactic sugar" $<-
        len1 <- unlist(lapply(value, length))==1
        if(any(len1)){
            for(i in which(len1)){
                vl <- value[[i]]
                vl <- ifelse(.is_replicable(vl), vl, list(vl))
                value[[i]] <- rep(vl, igraph::ecount(g))
            }
        }
        igraph::edge_attr(graph = g) <- value
    } else {
        if(length(value)==1){
            value <- ifelse(.is_replicable(value), value, list(value))
        }
        igraph::edge_attr(graph = g, name = name, ...=...) <- value  
    }
    x <- .updateGraphSpace(x, g)
    return(x)
})

.updateGraphSpace <- function(x, g) {
    x@graph <- .validate_igraph(g)
    x@edges <- .get_edges(x@graph)
    temp <- .center_nodes(.get_nodes(x@graph), x@misc$image, x@pars$mar)
    x@nodes <- temp$nodes
    x@image <- temp$image
    return(x)
}


