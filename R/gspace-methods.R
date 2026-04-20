
#-------------------------------------------------------------------------------
#' @title Create a GraphSpace object
#' 
#' @description \code{GraphSpace} is the main constructor for 
#' \linkS4class{GraphSpace} objects, designed to store graph data and 
#' metadata for optimized rendering in RGraphSpace.
#'
#' @param g An \link[igraph]{igraph} object. It must include vertex 
#' coordinates assigned to \code{x} and \code{y} attributes, and
#' vertex labels assigned to \code{name} attribute.
#' @param layout An optional numeric matrix with two columns for \code{x} and
#' \code{y} vertex coordinates. If provided, it overrides coordinates in \code{g}.
#' @param verbose A logical value. If \code{TRUE}, displays detailed messages.
#' @param mar `r lifecycle::badge("deprecated")` Deprecated since
#' RGraphSpace 1.1.1; use \link{normalizeGraphSpace} instead.
#' @param image `r lifecycle::badge("deprecated")` Deprecated since
#' RGraphSpace 1.1.1; use \link{normalizeGraphSpace} instead.
#' 
#' @return A \linkS4class{GraphSpace} class object.
#' 
#' @details
#' \code{GraphSpace} objects are designed to bridge the gap between network 
#' analysis (via \code{igraph}) and high-quality visualization (via \code{ggplot2}). 
#' The constructor ensures that all necessary aesthetics for 
#' \code{\link{geom_graphspace}} are pre-processed and validated.
#' 
#' \strong{Coordinate System and Normalization:}
#' By default, the constructor expects coordinates in the \code{x} and \code{y} 
#' vertex attributes, along with unique IDs in the \code{name} vertex 
#' attribute. If these are not provided, the constructor will generate 
#' sequential IDs and assign a layout using the 
#' \code{\link[igraph]{layout_nicely}} function. These coordinates define the 
#' relative positioning of nodes. For optimal rendering, it is recommended 
#' to pass the object through \code{\link{normalizeGraphSpace}} after 
#' construction. This converts vertex positions to Normalized Parent Coordinates 
#' (NPC), ensuring the graph remains centered and scaled relative to the 
#' plotting area.
#' 
#' \strong{Data Structure:}
#' The resulting object stores nodes and edges in separate internal slots, 
#' preserving metadata such as \code{nodeSize} and \code{edgeLineColor}. 
#' If an \code{igraph} object is provided without specific styling attributes, 
#' \code{GraphSpace} will assign the default values defined in the 
#' \code{\link{geom_graphspace}} aesthetics. Users can also specify custom 
#' variables in the input graph to be used as aesthetics within the 
#' \code{ggplot2} grammar.
#' 
#' \strong{Arrowhead Mapping:}
#' The \code{arrowType} attribute (see \emph{Arrowhead types} section) 
#' allows for a mapping between symbolic aliases (such as \code{"-->"}) 
#' and internal integer codes. This is useful for assigning interaction 
#' types in directed or undirected graphs (e.g., activation vs. inhibition).
#'  
#' @section Vertex attributes:
#' The following attributes in \code{g} are evaluated by the constructor:
#' 
#' \tabular{ll}{
#'   \code{nodeSize} \tab Numeric \code{[0, 100]}, representing % of the plotting space. \cr
#'   \code{nodeShape} \tab Integer code \code{[0-25]}; see \link[graphics]{points}. \cr
#'   \code{nodeColor} \tab A valid color name or hexadecimal code. \cr
#'   \code{nodeLineWidth} \tab Border thickness; see \link[grid]{gpar}. \cr
#'   \code{nodeLineColor} \tab A valid color name or hexadecimal code. \cr
#'   \code{nodeLabel} \tab Character string (\code{NA} will omit labels). \cr
#'   \code{nodeLabelSize} \tab Font size in \code{pts}; see \link[grid]{gpar}. \cr
#'   \code{nodeLabelColor} \tab A valid color name or hexadecimal code.
#' }
#' 
#' @section Edge attributes:
#' The following attributes in \code{g} are evaluated by the constructor:
#' 
#' \tabular{ll}{
#'   \code{edgeLineWidth} \tab Edge thickness; see \code{\link[grid]{gpar}}. \cr
#'   \code{edgeLineColor} \tab A valid color name or hexadecimal code. \cr
#'   \code{edgeLineType}  \tab Line style (e.g., "solid", "dashed"); see \code{\link[grid]{gpar}}. \cr
#'   \code{arrowType}     \tab Arrowhead style (see \emph{Arrowhead types} section).
#' }
#' 
#' @section Arrowhead types:
#' 
#' Arrowheads are controlled via the \code{arrowType} attribute using 
#' integer or character codes (see examples in the \emph{RGraphSpace} vignette).
#' 
#' In directed graphs, arrows follow the edge list orientation by default, 
#' representing forward directions (\emph{e.g.}, \code{A -> B}). 
#' While undirected graphs do not show arrows by default, specific styles 
#' can be manually assigned for detailed visualization, including forward, 
#' backward, or bidirectional arrowheads.
#' 
#' \subsection{Directed graphs (A -> B):}{
#' \tabular{lll}{
#'   \strong{Code} \tab \strong{Alias} \tab \strong{Description} \cr
#'   \code{0} \tab \code{"---"} \tab No arrow \cr
#'   \code{1} \tab \code{"-->"} \tab Forward arrow \cr
#'   \code{-1} \tab \code{"--|"} \tab Forward bar
#' }
#' }
#' 
#' \subsection{Undirected graphs (A -- B):}{
#' \tabular{lll}{
#'   \strong{Code} \tab \strong{Alias} \tab \strong{Description} \cr
#'   \code{0} \tab \code{"---"} \tab No arrow \cr
#'   \code{1} \tab \code{"-->"} \tab Forward arrow \cr
#'   \code{2} \tab \code{"<--"} \tab Backward arrow \cr
#'   \code{3} \tab \code{"<->"} \tab Bidirectional arrow \cr
#'   \code{4} \tab \code{"|->"} \tab Forward arrow / backward bar \cr
#'   \code{-1} \tab \code{"--|"} \tab Forward bar \cr
#'   \code{-2} \tab \code{"|--"} \tab Backward bar \cr
#'   \code{-3} \tab \code{"|-|"} \tab Bidirectional bar \cr
#'   \code{-4} \tab \code{"<-|"} \tab Backward arrow / forward bar
#' }
#' }
#' 
#' @author Sysbiolab.
#' 
#' @seealso \code{\link{geom_graphspace}}, \code{\link{plotGraphSpace}}
#' 
#' @examples
#' library(igraph)
#' 
#' # Create a star graph
#' gtoy1 <- make_full_graph(15)
#' 
#' # Custom attributes
#' V(gtoy1)$nodeSize <- 5
#' E(gtoy1)$edgeLineColor <- "red"
#' E(gtoy1)$arrowType <- "-->"
#' 
#' # Create a GraphSpace
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
GraphSpace <- function(g, layout = NULL, verbose = TRUE,  
    mar = deprecated(), image = deprecated()) {
    ### deprecate
    if (lifecycle::is_present(mar)) {
        deprecate_soft("1.1.1", "GraphSpace(mar)", 
            "normalizeGraphSpace(mar)")
    }
    if (lifecycle::is_present(image)) {
        deprecate_soft("1.1.1", "GraphSpace(image)", 
            "normalizeGraphSpace(image)")
    }
    .validate_gs_args("singleLogical", "verbose", verbose)
    #--- validate argument values
    if(!is.null(layout)){
        .validate_gs_args("numeric_mtx", "layout", layout)
        if (ncol(layout) != 2) {
            stop("'layout' matrix should have two columns.", call. = FALSE)
        } 
    }
    #--- validate igraph and build a gs object
    gs <- .buildGraphSpace(g, layout, verbose)
    
    return(gs)
}

#-------------------------------------------------------------------------------
#' @title Wrapper function to plot GraphSpace objects in ggplot2
#'
#' @description 
#' \code{plotGraphSpace()} is a High-level plotting interface that translates 
#' \code{igraph} and \code{GraphSpace} data objects into \code{ggplot2} layers.
#'
#' @param gs Either an \code{igraph} or \linkS4class{GraphSpace} class object.
#' If \code{gs} is an \code{igraph}, then it must include \code{x}, \code{y}, 
#' and \code{name}  vertex attributes (see \code{\link{GraphSpace}}).
#' @param theme Name of a custom RGraphSpace theme. These themes 
#' (from 'th0' to 'th3') consist of preconfigured ggplot settings, 
#' which can subsequently refine using \code{\link[ggplot2]{ggplot2}}.
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
#' library(igraph)
#' 
#' # Load a demo igraph
#' data('gtoy1', package = 'RGraphSpace')
#' 
#' # Generate a ggplot for gtoy1
#' plotGraphSpace(gtoy1)
#' 
#' # Create a star graph
#' gtoy_star <- make_full_graph(15)
#' 
#' # Example of setting node and edge attributes
#' V(gtoy_star)$nodeSize <- 5
#' E(gtoy_star)$edgeLineColor <- "red"
#' E(gtoy_star)$arrowType <- "<->"
#' 
#' # Create a GraphSpace object
#' gs_star <- GraphSpace(gtoy_star)
#' 
#' # Normalize graph coordinates
#' gs_star <- normalizeGraphSpace(gs_star)
#' 
#' # Generate a ggplot for gs_star
#' plotGraphSpace(gs_star)
#' 
#' @import methods
#' @importFrom ggplot2 ggplot
#' @importFrom grDevices col2rgb
#' @importFrom ggrastr rasterize
#' @docType methods
#' @rdname plotGraphSpace-methods
#' @aliases plotGraphSpace
#' @export
#'
setMethod("plotGraphSpace", "GraphSpace", 
    function(gs, theme = "th0", xlab = "Graph coordinates 1", 
        ylab = "Graph coordinates 2", font.size = 1,
        bg.color = "grey95", add.labels = FALSE,
        node.labels = NULL, label.size = 3, 
        label.color = "grey20", add.image = FALSE, 
        raster = FALSE, dpi = 300, dev = "cairo_png") {
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
        
        theme <- match.arg(theme, choices = c("th0", "th1", "th2", "th3"))
        
        #--- get gs slots
        nodes <- gs_nodes(gs)
        pars <- getGraphSpace(gs, "pars")
        
        #--- initialize a ggplot object
        ggp <- ggplot()
        
        #--- add image
        if(pars$image.layer){
            img <- getGraphSpace(gs, "image")
            if(add.image){
                ggp <- .add_image(ggp, img)
            } else {
                ggi <- .add_image(ggp, img)
                ggi <- ggi + theme_gspace_coords(theme = theme, 
                  is_norm = pars$is.normalized, xlab = xlab, ylab = ylab, 
                  txt_size = font.size, leg_size = font.size, 
                  bg_color = bg.color)
            }
        }

        #--- add graph
        ggp <- ggp + geom_graphspace(data = gs)
        
        #--- add node labels
        if (!is.null(node.labels)){
            ggp <- .add_labels1(ggp, nodes, node.labels, 
                label.size, label.color)
        } else if(add.labels){
            ggp <- .add_labels2(ggp, nodes)
        }
        
        #--- apply custom theme
        ggp <- ggp + theme_gspace_coords(theme = theme, 
          is_norm = pars$is.normalized, xlab = xlab, ylab = ylab, 
          txt_size = font.size, leg_size = font.size,
          bg_color = bg.color)
        
        if(raster){
          ggp <- ggrastr::rasterize(ggp, layers = "GraphSpace", 
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
#' @import methods
#' @docType methods
#' @rdname plotGraphSpace-methods
#' @aliases plotGraphSpace
#' @export
#'
setMethod("plotGraphSpace", "igraph", 
    function(gs, ...) {
        gs <- GraphSpace(gs, verbose=FALSE)
        gs <- normalizeGraphSpace(gs)
        plotGraphSpace(gs = gs, ...)
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
    x@nodes
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_edges", "GraphSpace", function(x) {
    .get_gs_edges(x)
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
    x <- .updateNodeSpace(x, g)
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
    x <- .updateEdgeSpace(x, g)
    return(x)
})
.updateEdgeSpace <- function(x, g){
  x@graph <- .validate_igraph(g)
  x@edges <- .get_edges(x@graph)
  return(x)
}
.updateNodeSpace <- function(x, g) {
    x@graph <- .validate_igraph(g)
    pars <- x@pars
    if(pars$is.normalized){
        if(pars$image.layer){
            x <- normalizeGraphSpace(x, 
                image = x@misc$image,
                mar = pars$mar,
                flip.x = pars$flip.x %||% FALSE, 
                flip.y = pars$flip.y %||% FALSE, 
                rotate.xy = pars$rotate.xy %||% FALSE, 
                flip.v = pars$flip.v %||% TRUE, 
                flip.h = pars$flip.h %||% FALSE,
                verbose = FALSE)
        } else {
            x <- normalizeGraphSpace(x, 
                mar = pars$mar,
                flip.x = pars$flip.x %||% FALSE, 
                flip.y = pars$flip.y %||% FALSE, 
                rotate.xy = pars$rotate.xy %||% FALSE,
                verbose = FALSE)
        }
    } else {
      x@nodes <- .get_nodes(x@graph)
    }
    return(x)
}

# .recenter_nodes <- function(nodes, image, mar){
#     if(is.null(image)){
#         temp_list <- .center_graph_nodes(nodes, mar)
#     } else {
#         temp_list <- .adjust_image_nodes(nodes, image, mar)
#     }
#     return(temp_list)
# }

