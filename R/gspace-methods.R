
#-------------------------------------------------------------------------------
#' @title Create a GraphSpace object
#' 
#' @description \code{GraphSpace} is the main constructor for 
#' \linkS4class{GraphSpace} objects, designed to store graph data and 
#' metadata for optimized rendering in RGraphSpace.
#'
#' @param g A graph object inheriting from the \link[igraph]{igraph} class 
#' (such as \code{igraph} and \code{tbl_graph}) or a \code{data.frame} used 
#' to initialize a \code{GraphSpace} object. If a graph is provided, it 
#' should include vertex coordinates in \code{x} and \code{y} attributes, 
#' and vertex labels in the \code{name} attribute. If a \code{data.frame} 
#' is provided, it must contain at least \code{x} and \code{y} columns 
#' representing the node coordinates; additional columns will be treated as 
#' vertex attributes. For graphs requiring edge definitions, use the 
#' \code{igraph} initialization.
#' @param layout An optional numeric matrix with two columns for \code{x} and
#' \code{y} vertex coordinates. If provided, it overrides coordinates in \code{g}.
#' @param simplify A logical value. If \code{TRUE} (default), removes loops and 
#' multiple edges (see \link[igraph]{simplify}).
#' @param verbose A logical value. If \code{TRUE} (default), displays detailed 
#' messages.
#' @param ... Additional arguments passed to the \code{GraphSpace} constructor.
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
#' preserving metadata such as \code{nodeSize} and \code{edgeColor}. 
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
#'   \code{edgeColor} \tab A valid color name or hexadecimal code. \cr
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
#' library(RGraphSpace)
#' library(igraph)
#' 
#' # Create a star graph
#' gtoy1 <- make_full_graph(15)
#' 
#' # Custom attributes
#' V(gtoy1)$nodeSize <- 5
#' E(gtoy1)$edgeColor <- "red"
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
#' @importFrom igraph ends delete_vertex_attr 'edge_attr<-' as.igraph
#' @importFrom tidygraph as.igraph
#' @importFrom scales rescale
#' @importFrom grDevices is.raster as.raster
#' @aliases GraphSpace
#' @rdname GraphSpace-methods
#' @export
setMethod("GraphSpace", signature(g = "ANY"),
  function(g, layout = NULL, simplify = TRUE, verbose = TRUE, ...) {
    
    #--- validate argument values
    .validate_gs_args("singleLogical", "simplify", simplify)
    .validate_gs_args("singleLogical", "verbose", verbose)
    if(inherits(g, "layout_ggraph")){
      if (!inherits(attr(g, "graph"), "igraph")) {
        rlang::abort(
          message = c(
            "x" = "The 'layout_ggraph' object is missing a valid 'graph' attribute.",
            "i" = "RGraphSpace requires an 'igraph' object to be embedded in the layout.",
            "*" = "Ensure you are using a standard `ggraph::create_layout()` object."
          )
        )
      }
      layout <- tryCatch(
        as.matrix(g[, c("x", "y")]),
        error = function(e) NULL
      )
      g <- attr(g, "graph")
    } else if (inherits(g, "tbl_graph")) {
      g <- tidygraph::as.igraph(g)
    }
    
    if (!inherits(g, "igraph")) {
      rlang::abort(
        message = c(
          "x" = "Input 'g' must inherit from the 'igraph' class.",
          "i" = "Input must be an 'igraph', 'tbl_graph', or 'layout_ggraph'."
        )
      )
    }
    class(g) <- "igraph"
    
    if(!is.null(layout)){
      .validate_gs_args("numeric_mtx", "layout", layout)
      if (ncol(layout) != 2) {
        rlang::abort(
          message = c(
            "x" = "The 'layout' matrix must have exactly two columns.",
            "i" = paste("Received a matrix with", ncol(layout), "columns."),
            "*" = "Ensure 'layout' represents (x, y) coordinates."
          )
        )
      } 
    }
    
    #--- validate igraph and build a gs object
    gs <- .buildGraphSpace(g, layout, simplify, verbose)
    
    return(gs)
  }
)

#' @rdname GraphSpace-methods
#' @export
setMethod("GraphSpace", signature(g = "data.frame"),
  function(g, verbose = TRUE, ...){
    
    if(!all(c("x", "y") %in% colnames(g))){
      ms_i <- c("i" = "GraphSpace requires x/y columns for 'data.frame' initialization.")
      rlang::abort(
        message = c("x" = "Input 'g' is missing 'x' and 'y' coordinates."),
        body = ms_i
      )
    }
    
    g <- .graphFromCoordinates(g)
    
    #--- build GraphSpace-class
    gs <- GraphSpace(g = g, verbose = verbose, ...)
    
    return(gs)
    
  }
)

#-------------------------------------------------------------------------------
.graphFromCoordinates <- function(coord){
  
  # Check attributes
  attr <- unique(colnames(coord))
  attr <- attr[!is.na(attr)]
  attr <- attr[!attr%in%c("x","y")]
  
  # Initialize a graph using 'coord' as vertices, with no edges
  g <- make_empty_graph(n = nrow(coord), directed = FALSE)
  
  # If 'coord' has no row names (a common case for a plain data.frame), 
  # sequential names will be later assigned in the validation functions
  if(!is.null(rownames(coord))){
    V(g)$name <- rownames(coord)
  }
  
  # Add coordinates
  V(g)$x <- coord$x
  V(g)$y <- coord$y
  
  if(length(attr)>0){
    for(name in attr){
      igraph::vertex_attr(g, name) <- coord[[name]]
    }
  }
  
  return(g)
  
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
#' which can be subsequently refine using \code{\link[ggplot2]{theme}}.
#' @param xlab The title for the 'x' axis of a 2D-image space.
#' @param ylab The title for the 'y' axis of a 2D-image space.
#' @param node.labels Optional specification of node labels to display.
#' If \code{FALSE} (default), no labels are shown. If \code{TRUE}, labels are 
#' displayed for all nodes. Otherwise, a character vector may be supplied 
#' to display labels only for the specified nodes. Character values are matched 
#' against the \code{nodeLabel} attribute.
#' @param label.size Font size passed to \code{\link[ggplot2]{geom_text}}.
#' @param label.color Color passed to \code{\link[ggplot2]{geom_text}}.
#' @param add.image A logical value indicating whether to add a background 
#' image, when one is available (see \code{\link{GraphSpace}}).
#' @param raster A logical value indicating whether to rasterize the main plot.
#' See \code{\link[ggrastr]{rasterise}} for further specifications.
#' @param dpi Raster resolution, in dots per inch.
#' @param dev Device used in the \code{\link[ggrastr]{rasterise}} call.
#' @param add.labels Deprecated. Use \code{node.labels} instead.
#' @param font.size Deprecated. Use \code{\link[ggplot2]{theme}} customization instead.
#' @param bg.color Deprecated. Use \code{\link[ggplot2]{theme}} customization instead.
#' 
#' @return A ggplot-class object.
#' @author Sysbiolab.
#' @seealso \code{\link{GraphSpace}}
#' @examples
#' library(RGraphSpace)
#' library(igraph)
#' 
#' # Load a demo igraph
#' data('gtoy1', package = 'RGraphSpace')
#' 
#' # Generate a ggplot for gtoy1
#' plotGraphSpace(gtoy1, node.labels = TRUE)
#' 
#' 
#' # Create a star graph
#' gtoy_star <- make_full_graph(15)
#' 
#' # Example of setting node and edge attributes
#' V(gtoy_star)$nodeSize <- 5
#' E(gtoy_star)$edgeColor <- "red"
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
#' @importFrom ggplot2 ggplot geom_text
#' @importFrom grDevices col2rgb
#' @importFrom ggrastr rasterize
#' @docType methods
#' @rdname plotGraphSpace-methods
#' @aliases plotGraphSpace
#' @export
setMethod("plotGraphSpace", "GraphSpace", 
  function(gs, theme = "th0", xlab = "Graph coordinates 1", 
    ylab = "Graph coordinates 2", node.labels = FALSE, label.size = 3, 
    label.color = "grey20", add.image = TRUE, raster = FALSE, dpi = 300, 
    dev = "cairo_png", add.labels = deprecated(), font.size = deprecated(), 
    bg.color = deprecated()) {
    
    if (lifecycle::is_present(add.labels)) {
      lifecycle::deprecate_soft("1.4.2", "plotGraphSpace(add.labels)",
        details = "Use `plotGraphSpace(node.labels)` instead.")
      node.labels <- add.labels
    }
    if (lifecycle::is_present(font.size)) {
      lifecycle::deprecate_soft("1.4.2", "plotGraphSpace(font.size)",
        details = "Use `theme(...)` instead.")
    }
    if (lifecycle::is_present(bg.color)) {
      lifecycle::deprecate_soft("1.4.2", "plotGraphSpace(bg.color)",
        details = "Use `theme(...)` instead.")
    }
    
    gs <- updateGraphSpace(gs)
    
    #--- validate the gs object and args
    .validate_gs_args("singleString", "xlab", xlab)
    .validate_gs_args("singleString", "ylab", ylab)
    .validate_gs_args("singleLogical", "add.image", add.image)
    .validate_gs_args("singleNumber", "label.size", label.size)
    .validate_gs_colors("singleColor", "label.color", label.color)
    .validate_gs_args("singleLogical", "raster", raster)
    .validate_gs_args("singleInteger", "dpi", dpi)
    .validate_gs_args("singleString", "dev", dev)
    
    theme <- match.arg(theme, choices = c("th0", "th1", "th2", "th3"))
    
    label_aes <- aes(label = NA_character_)
    if ("nodeLabel" %in% gs_names(gs) && 
        (isTRUE(node.labels) || is.character(node.labels))) {
      
      gs$plotLabel <- gs$nodeLabel
      
      if (is.character(node.labels)) {
        .validate_gs_args("allCharacter", "node.labels", node.labels)
        gs$plotLabel[!gs$plotLabel %in% node.labels] <- NA_character_
      }
      plotLabel <- NULL
      label_aes <- aes(label = plotLabel)
    }
    
    if(missing(label.size)){
      label.size <- gs$nodeLabelSize %||% label.size
    }
    
    if(missing(label.color)){
      label.color <- gs$nodeLabelColor %||% label.color
    }
    
    #--- initialize a ggplot object
    if(.has_image(gs) && add.image){
      ggp <- ggplot(gs) + 
        annotation_gspace_image(gs) +
        geom_edgespace(raster = raster, dpi = dpi, dev = dev) +
        geom_nodespace(mapping = label_aes, label_size = label.size, 
          label_colour = label.color, raster = raster, 
          dpi = dpi, dev = dev)
    } else {
      ggp <- ggplot(gs) + 
        geom_edgespace(raster = raster, dpi = dpi, dev = dev) +
        geom_nodespace(mapping = label_aes, label_size = label.size,
          label_colour = label.color, raster = raster, 
          dpi = dpi, dev = dev)
    }
    
    #--- apply custom theme
    ggp <- ggp + theme_gspace_coords(theme = theme, 
      is_norm = .is_normalized(gs), xlab = xlab, ylab = ylab)
    
    return(ggp)
    
  }
)

.plot_graph_wrapper <- function(gs, ...) {
  gs <- GraphSpace(gs, verbose = FALSE)
  gs <- normalizeGraphSpace(gs, verbose = FALSE)
  plotGraphSpace(gs = gs, ...)
}

#' @param ... Additional arguments passed to the 
#' \code{\link{plotGraphSpace}} function.
#' @import methods
#' @docType methods
#' @rdname plotGraphSpace-methods
#' @aliases plotGraphSpace
#' @export
setMethod("plotGraphSpace", signature(gs = "igraph"), .plot_graph_wrapper)

#' @rdname plotGraphSpace-methods
#' @aliases plotGraphSpace
#' @export
setMethod("plotGraphSpace", signature(gs = "tbl_graph"), .plot_graph_wrapper)

#' @rdname plotGraphSpace-methods
#' @aliases plotGraphSpace
#' @export
setMethod("plotGraphSpace", signature(gs = "gs_graph"), .plot_graph_wrapper)

#-------------------------------------------------------------------------------
#' Plot GraphSpace objects
#' 
#' @param x A \linkS4class{GraphSpace} class object.
#' @param ... Additional arguments passed to the 
#' \code{\link{plotGraphSpace}} function.
#' @seealso \code{\link{plotGraphSpace}}
#' 
#' @importFrom graphics plot
#' @export
plot.GraphSpace <- function(x, ...) {
    plotGraphSpace(x, ...)
}

#-------------------------------------------------------------------------------
#' @title Accessors for fetching slots from a GraphSpace object
#'
#' @description \code{getGraphSpace} retrieves information from
#' individual slots available in a GraphSpace object.
#'
#' @param gs A preprocessed \linkS4class{GraphSpace} class object
#' @param what A single character value specifying which slot to 
#' retrieve from a 'GraphSpace' object.
#' Options: "graph", "nodes", "edges", "pars", "misc", "image", 
#' "canvas", and "fdata".
#' @return Content from slots in the \linkS4class{GraphSpace} object.
#' @examples
#' library(RGraphSpace)
#' library(igraph)
#' 
#' # Load a demo igraph
#' data('gtoy1', package = 'RGraphSpace')
#'
#' # Create a new GraphSpace object
#' gs <- GraphSpace(gtoy1)
#'
#' # Get the 'graph' slot in gs
#' getGraphSpace(gs, what = 'graph')
#'
#' @import methods
#' @docType methods
#' @rdname getGraphSpace-methods
#' @aliases getGraphSpace
#' @export
setMethod("getGraphSpace", "GraphSpace", function(gs, what = "graph") {
  .validate_gs_args("singleString", "what", what)
  opts <- c("graph", "nodes", "edges", "pars", "misc", "image", "canvas","fdata")
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
    .check_outdated_gs(gs, c("image", "canvas"), type = "abort")
    obj <- gs@image
  } else if (what == "canvas") {
    .check_outdated_gs(gs, c("image", "canvas"), type = "abort")
    obj <- gs@canvas
  } else if (what == "fdata") {
    .check_outdated_gs(gs, "fdata", type = "abort")
    obj <- gs@fdata
  } else {
    obj <- gs@graph
  }
  return(obj)
})

#-------------------------------------------------------------------------------
#' @title Accessors and attribute utilities for GraphSpace objects
#' 
#' @description Access and modify individual components of a
#' \linkS4class{GraphSpace} object. Selected \pkg{igraph} methods are
#' applied to the internal graph representation and propagated to
#' downstream node and edge components.
#' 
#' @param x A \linkS4class{GraphSpace} class object
#' @param name Name of the attribute.
#' @param value Replacement value for the selected slot or attribute.
#' @param ... Additional arguments passed to extraction methods. 
#' @details
#' For \code{gs_nodes()}, the optional \code{vars} argument specifies
#' node-associated features retrieved from the \code{fdata}
#' container. See also \code{\link{gs_fetch_features}}.
#' @return Updated \linkS4class{GraphSpace} object.
#' @seealso \code{\link[igraph]{vertex_attr}}, \code{\link[igraph]{edge_attr}}, 
#' \code{\link{gs_fetch_features}}
#' @examples
#' library(RGraphSpace)
#' library(igraph)
#' 
#' # Load a demo igraph
#' data('gtoy1', package = 'RGraphSpace')
#' 
#' # Create a new GraphSpace object
#' gs <- GraphSpace(gtoy1)
#' 
#' #--- Usage of GraphSpace attribute accessors:
#' 
#' # Vertex names
#' names(gs)
#' 
#' # Vertex attribute names
#' gs_names(gs)
#' 
#' # Get a data frame with nodes
#' gs_nodes(gs)
#' 
#' # Get a data frame with edges
#' gs_edges(gs)
#' 
#' # Get vertex count
#' gs_vcount(gs)
#' 
#' # Get edge count
#' gs_ecount(gs)
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
#' # Access a specific edge attribute
#' gs_edge_attr(gs, "edgeColor")
#' 
#' # Replace an entire edge attribute
#' gs_edge_attr(gs, "edgeLineWidth") <- 1
#' 
#' # Add an image and rescale graph coordinates to image space
#' # Images may be provided as a raster or numeric matrix
#' gs_image(gs) <- as_colorraster(volcano)
#' gs <- normalizeGraphSpace(gs, image.space = FALSE)
#' 
#' @name GraphSpace-accessors
#' @aliases names
#' @aliases gs_names
#' @aliases gs_nodes
#' @aliases gs_edges
#' @aliases gs_graph
#' @aliases gs_image
#' @aliases gs_image<-
#' @aliases gs_fdata
#' @aliases gs_fdata<-
#' @aliases gs_features
#' @aliases gs_nfeatures
#' @aliases gs_vcount
#' @aliases gs_ecount
#' @aliases gs_vertex_attr
#' @aliases gs_vertex_attr<-
#' @aliases gs_edge_attr
#' @aliases gs_edge_attr<-
NULL

#' @rdname GraphSpace-accessors
#' @aliases names,GraphSpace-method
#' @export
setMethod("names", "GraphSpace", function(x) {
  x@nodes$name
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_names", "GraphSpace", function(x) {
  colnames(x@nodes)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_nodes", "GraphSpace", function(x, ...) {
  
  args <- list(...)
  
  vars <- args$vars %||% FALSE
  
  render <- args$render %||% FALSE
  
  nodes <- if (isTRUE(render)) .gs_nodes(x) else x@nodes
  
  if (.all_characterValues(vars)) {
    
    signal_df <- gs_fetch_features(x, vars = vars, as_df = TRUE)
    
    if (!is.null(signal_df)) {
      
      signal_vars <- setdiff(colnames(signal_df), colnames(nodes) )
      
      if (length(signal_vars) > 0) {
        signal_df <- signal_df[ rownames(nodes), signal_vars, drop = FALSE]
        nodes[, signal_vars] <- signal_df
      }
      
    }
    
  }
  
  if (render) {
    attr(nodes, "gs_handler_type") <- "node"
    attr(nodes, "gs_id") <- x@uuid
    class(nodes) <- c("gs_nodes", class(nodes))
  }
  
  return(nodes)
  
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_edges", "GraphSpace", function(x, ...) {
  
  args <- list(...)
  
  render <- args$render %||% FALSE
  
  if(isFALSE(render)) return(x@edges)
  
  edges <- .gs_edges(x)
  attr(edges, "gs_id") <- x@uuid
  attr(edges, "gs_handler_type") <- "edge"
  class(edges) <- c("gs_edges", class(edges))
  return(edges)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_image", "GraphSpace", function(x) {
  
  .check_outdated_gs(x, c("image", "canvas"), type = "abort")
  
  x <- .get_canvas(x)
  attr(x, "gs_handler_type") <- "image"
  class(x) <- c("gs_image", class(x))
  return(x)
})

#' @rdname GraphSpace-accessors
#' @export
setReplaceMethod("gs_image", "GraphSpace", function(x, value) {

  .check_outdated_gs(x, c("image", "canvas"), type = "abort")
  
  if(!is.raster(value)){
    .validate_gs_args("numeric_mtx", "value", value)
    rlang::inform(
      c("i" = "Rasterizing numeric matrix.",
        "*" = "Values outside [0,1] are rescaled before conversion.")
    )
    rng <- range(value, na.rm = TRUE)
    if (diff(rng) == 0) {
      if (rng[1] < 0 || rng[1] > 1) {
        value[] <- 0
      }
    } else if (rng[1] < 0 || rng[2] > 1) {
      value <- (value - rng[1]) / diff(rng)
    }
    value <- as.raster(value)
  }
  
  .inform_image_boundaries(value)
  
  x@image <- value
  return(x)
})


#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_graph", "GraphSpace", function(x) {
  g <- x@graph
  attr(g, "gs_handler_type") <- "graph"
  class(g) <- c("gs_graph", class(g))
  return(g)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_fdata", "GraphSpace", function(x) {
  
  .check_outdated_gs(x, "fdata", type = "abort")
  
  x@fdata
  
})

#' @rdname GraphSpace-accessors
#' @export
setReplaceMethod("gs_fdata", "GraphSpace", function(x, value) {
  
  .check_outdated_gs(x, "fdata", type = "abort")
  
  x <- gs_add_features(x, value)
  
  return(x)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_nfeatures", "GraphSpace", function(x) {
  
  .check_outdated_gs(x, "fdata", type = "abort")
  
  ncol(x@fdata)
  
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_features", "GraphSpace", function(x) {
  
  .check_outdated_gs(x, "fdata", type = "abort")
  
  colnames(x@fdata)
  
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
setMethod("gs_vertex_attr", "GraphSpace", function(x, name, ..., value) {
  if(missing(value)){
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
  } else {
    gs_vertex_attr(x, name, ...) <- value
    return(x)
  }

})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_vertex_attr<-", "GraphSpace", function(x, name, ..., value) {
  
  # Check protected attributes
  if (name %in% .gs_protected_node_cols()) {
    rlang::abort(c(
      x = sprintf("'%s' is a read-only edge attribute.", name),
      i = "It is maintained internally and cannot be set directly.",
      "*" = "To change the graph structure, modify the underlying igraph object directly."
    ))
  }
  
  g <- x@graph
  if(length(value)==1){
    value <- if(.is_replicable(value)) value else list(value)
  }
  igraph::vertex_attr(graph = g, name = name, ...=...) <- value
  x <- .updateNodeSpace(x, g)
  
  return(x)
  
})

# Used to handle possible function replication
.is_replicable <- function(x) {
  tryCatch({
    rep(x, 2)
    TRUE
  }, error = function(e) FALSE)
}

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_edge_attr", "GraphSpace", function(x, name, ..., value) {
  if (missing(value)) {
    g <- x@graph
    if(missing(name)){
      att <- igraph::edge_attr(graph = g, ...=...)
    } else {
      att <- igraph::edge_attr(graph = g, name = name, ...=...)
    }
    return(att)
  } else {
    gs_edge_attr(x, name, ...) <- value
    return(x)
  }
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_edge_attr<-", "GraphSpace", function(x, name, ..., value) {
  
  # Check protected attributes
  if (name %in% .gs_protected_edge_cols()) {
    rlang::abort(c(
      x = sprintf("'%s' is a read-only edge attribute.", name),
      i = "It is maintained internally and cannot be set directly.",
      "*" = "To change the graph structure, modify the underlying igraph object directly."
    ))
  }
  
  g <- x@graph
  if(length(value)==1){
    value <- if(.is_replicable(value)) value else list(value)
  }
  igraph::edge_attr(graph = g, name = name, ...=...) <- value
  x <- .updateEdgeSpace(x, g)
  
  return(x)
  
})

.updateEdgeSpace <- function(x, g){
  x@graph <- .validate_igraph(g, simplify = .is_simplified(x))
  x@edges <- .get_edges(x@graph, simplify = .is_simplified(x))
  return(x)
}

.updateNodeSpace <- function(x, g) {
  x@graph <- .validate_igraph(g, simplify = .is_simplified(x))
  nodes <- .get_nodes(x@graph)
  if (.is_normalized(x)) {
    nodes[x@nodes$name, c("x","y")] <- x@nodes[, c("x","y")]
  }
  x@nodes <- nodes
  return(x)
}

#' @rdname GraphSpace-accessors
#' @aliases $,GraphSpace-method
#' @export
setMethod("$", "GraphSpace", function(x, name) {
  
  nodes <- x@nodes
  
  if (!(name %in% names(nodes))) {
    return(NULL)
  }
  
  nodes[[name]]
})

#' @rdname GraphSpace-accessors
#' @aliases $<-,GraphSpace-method [[<-,GraphSpace-method
#' @export
setReplaceMethod("$", "GraphSpace", function(x, name, value) {
  gs_vertex_attr(x, name) <- value
  x
})

#' @rdname GraphSpace-accessors
#' @method as.igraph GraphSpace
#' @export
as.igraph.GraphSpace <- function(x, ...) {
  return(x@graph)
}

################################################################################
### Internal for GraphSpace objects
################################################################################
#' Internal methods for GraphSpace
#' 
#' @description 
#' Exported solely to enable RStudio auto-completion 
#' and should not be called directly by the user.
#' 
#' @param x,pattern Internal arguments.
#' @keywords internal
#' @name GraphSpace-internal
NULL

#' @rdname GraphSpace-internal
#' @importFrom utils .DollarNames
#' @method .DollarNames GraphSpace
#' @keywords internal
#' @export
.DollarNames.GraphSpace <- function(x, pattern = "") {
  grep(pattern, names(x@nodes), value = TRUE)
}
