
#-------------------------------------------------------------------------------
#' @title Draw node and edge elements in a 2D graph layout
#' 
#' @description
#' 
#' Constructor for \link{GeomGraphSpace} ggproto objects.
#' 
#' A wrapper around \link[ggplot2]{geom_point} that enables direct use of
#' node attributes stored in \link{GraphSpace} objects as aesthetics.
#'
#' This geom is designed to map node-level attributes (e.g., \code{fill},
#' \code{size}) or any aesthetics supported by \link[ggplot2]{GeomPoint}.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' These mappings override global aesthetics and are not inherited 
#' from the top-level plot.
#'
#' @param data A \link{GraphSpace} object.
#'
#' @param stat The statistical transformation to use on the data.
#' Defaults to \code{identity}.
#'
#' @param position Position adjustment, either as a string or
#' the result of a call to a position adjustment function.
#'
#' @param ... Additional parameters passed to the underlying
#' drawing function in \link{GeomGraphSpace}.
#'
#' @param na.rm Logical. Should missing values be removed?
#' Defaults to \code{FALSE}.
#' 
#' @param show.legend Logical or a named logical vector indicating
#' whether this layer should be included in legends.
#'
#' @param inherit.aes Logical. If \code{FALSE} (default), the layer will use 
#' aesthetics defined in \code{mapping}.
#' 
#' @param arrow_size Numeric scaling factor controlling arrowhead 
#' geometry (see 'drawing' section).
#' 
#' @param arrow_offset Numeric value controlling the base offset of arrows  
#' at edge endpoints (see 'drawing' section).
#' 
#' @return A ggplot2 layer that renders node glyphs defined by
#' \link{GeomGraphSpace}.
#'
#' @section Aesthetics for node drawing:
#' 
#' Nodes are drawn in the main layer of \code{geom_graphspace()}, which 
#' understands \link[ggplot2]{geom_point} aesthetics.
#' 
#' If these aesthetics are not explicitly provided in \code{aes()}, they 
#' are automatically retrieved from the \link{GraphSpace} object.
#' 
#' \tabular{ll}{
#'   \strong{\code{x}, \code{y}, \code{vertex}} \tab  
#'   Required (automatically supplied).\cr
#'   \code{fill} \tab Node interior colour (see \link[ggplot2]{aes_colour_fill_alpha}).\cr
#'   \code{colour} \tab Node border colour (see \link[ggplot2]{aes_colour_fill_alpha}).\cr
#'   \code{alpha} \tab Transparency (see \link[ggplot2]{aes_colour_fill_alpha}).\cr
#'   \code{shape} \tab Node shape (see \link{points} and \link[ggplot2]{aes_linetype_size_shape}).\cr
#'   \code{size} \tab Node size (see *drawing* section and \link[ggplot2]{aes_linetype_size_shape}).\cr
#'   \code{stroke} \tab Node line width (see \link[ggplot2]{gg_par} and \link[ggplot2]{aes_linetype_size_shape}).
#' }
#' 
#' Required aesthetics \code{x}, \code{y}, and \code{vertex} are supplied from
#' the \link{GraphSpace} object and do not need to be manually mapped.
#' 
#' Additional parameters can be passed to control fixed values for the layer.
#' For example: `fill = "red"`, `stroke = 3`, `alpha = 0.5`, or `shape = 21`.
#' 
#' The interpretation of \strong{\code{size}} depends on how it is provided:
#' \itemize{
#'   \item **As an aesthetic**: When mapped within \code{aes()}, \code{size}
#'   follows the behavior of \link[ggplot2]{geom_point}, using absolute
#'   units to ensure consistency with the plot legends.
#'   \item **As a parameter**: When set outside \code{aes()}, \code{size} is 
#'   treated as a percentage of the viewport (\code{[0, 100]}), scaling 
#'   in \code{npc} units. This allows nodes to resize dynamically with 
#'   viewport changes.
#' }
#' 
#' @section Edge context-aware parameters:
#' 
#' These parameters control the edge appearance. If not explicitly provided, 
#' they are automatically retrieved from the \link{GraphSpace} object. 
#' They can be a single value or a vector matching the number of edges:
#' 
#' \tabular{ll}{
#'   \code{edge_colour} \tab Node border colour.\cr
#'   \code{edge_linetype} \tab Edge line type.\cr
#'   \code{edge_linewidth} \tab Edge line width.\cr
#'    \code{edge_alpha} \tab Edge transparency.
#' }
#' 
#' @section Edge global parameters:
#' 
#' These parameters apply globally to all edges in the layer:
#' 
#' \tabular{ll}{
#'    \code{arrow_size} \tab Arrow scaling factor (default = 1).\cr
#'    \code{arrow_offset} \tab Arrow offset from nodes (default = 0.01).\cr
#'    \code{arrow_lineend} \tab Line end style (see \link[grid]{gpar}).\cr
#'    \code{arrow_linejoin} \tab Line join style (see \link[grid]{gpar}).
#' }
#' 
#' **arrow_size** is a numeric scaling factor controlling arrowhead geometry. 
#' The value is interpreted in the same numeric space as line width (\code{lwd}), 
#' ensuring consistent scaling between edge strokes and arrowheads.
#' 
#' **arrow_offset** is an additive term that offsets arrow endpoints 
#' uniformly in graph space and is bounded by the edge length, in NPC units.
#' 
#' Arrowhead types are specified in the \link{GraphSpace} constructor.
#' 
#' @seealso
#' \link{GraphSpace}, \link[ggplot2]{geom_point}
#'
#' @examples
#' 
#' # Make a demo igraph
#' library(igraph)
#' gtoy1 <- make_star(15, mode="out")
#' 
#' # Set some node attributes
#' V(gtoy1)$nodeSize <- runif(vcount(gtoy1), 1, 20)
#' V(gtoy1)$nodeColor <- rainbow(vcount(gtoy1))
#' 
#' # Set some variables
#' V(gtoy1)$user_var1 <- runif(vcount(gtoy1), 1, 3)^3
#' V(gtoy1)$user_var2 <-  rep(c(1, 2, 3), each = 5)
#' 
#' # Create a GraphSpace object
#' gs <- GraphSpace(gtoy1, layout = layout_in_circle(gtoy1))
#' 
#' \dontrun{
#' 
#' # Example 1: Nodes scaling with the legend
#' # When 'size' is mapped inside aes(), it follows
#' # ggplot2 default behavior: sizes are translated 
#' # to absolute units (mm) via 'scale_size()'.
#' 
#' ggplot() + 
#'   geom_graphspace(
#'   mapping = aes(size = nodeSize, fill = user_var2), 
#'   data = gs, arrow_offset = 0.01) + 
#'   scale_size(range = c(1, 12)) + 
#'   theme(aspect.ratio = 1)
#'   
#' # Example 2: Nodes scaling with the viewport
#' # When 'size' is passed as a node attribute, 
#' # as inherited from the igraph object, it is
#' # interpreted as a percentage of the plot area
#' # and translated to NPC units.
#' 
#' ggplot() +
#'   geom_graphspace(mapping = aes(fill = user_var2), 
#'   data = gs, arrow_offset = 0.01) + 
#'   theme(aspect.ratio = 1)
#'   
#' }
#' 
#' @export
geom_graphspace <- function(mapping = NULL, data = NULL, 
  stat = "identity", position = "identity", ...,
  na.rm = FALSE, show.legend = NA, inherit.aes = FALSE,
  arrow_size = 1, arrow_offset = 0.01) {
  
  if (inherits(data, "GraphSpace")) {
    .geom_check_slots(data)
    nodes <- gs_nodes(data)
    edges <- gs_edges(data)
  } else {
    stop("'data' must be a 'GraphSpace' object.")
  }
  
  params <- rlang::list2(
    na.rm = na.rm,
    edges = edges,
    arrow_size = arrow_size,
    arrow_offset = arrow_offset,
    ...)
  
  mapping <- .mapping_graphspace(mapping)
  
  params <- .params_graphspace(params, mapping, nodes, edges)
  
  ggplot2::layer(
    geom = GeomGraphSpace,
    mapping = mapping,
    data = nodes,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
  
}

#-------------------------------------------------------------------------------
.mapping_graphspace <- function(mapping) {
  x <- y <- vertex <- NULL
  default_mapping <- ggplot2::aes(x = x, y = y, vertex = vertex)
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- utils::modifyList(default_mapping, mapping)
  }
  return(mapping)
}

#-------------------------------------------------------------------------------
.params_graphspace <- function(params, mapping, nodes, edges){
  
  params$.size_unit <- if("size" %in% names(mapping)) "mm" else "npc"
  
  #--- nodes
  
  if(is.null(params[["size"]]) && is.null(mapping[["size"]])){
    if("nodeSize" %in% names(nodes) ){
      params[["size"]] <- nodes[["nodeSize"]]
    }
  }
  
  if(is.null(params[["stroke"]]) && is.null(mapping[["stroke"]])){
    if("nodeLineWidth" %in% names(nodes) ){
      params[["stroke"]] <- nodes[["nodeLineWidth"]]
    }
  }
  
  if(is.null(params[["shape"]]) && is.null(mapping[["shape"]])){
    if("nodeShape" %in% names(nodes) ){
      params[["shape"]] <- nodes[["nodeShape"]]
    }
  }
  
  if(is.null(params[["fill"]]) && is.null(mapping[["fill"]])){
    if("nodeColor" %in% names(nodes) ){
      params[["fill"]] <- nodes[["nodeColor"]]
    }
  }
  
  if(is.null(params[["colour"]]) && is.null(mapping[["colour"]])){
    if("nodeLineColor" %in% names(nodes) ){
      params[["colour"]] <- nodes[["nodeLineColor"]]
    }
  }
  
  if(is.null(params[["alpha"]]) && is.null(mapping[["alpha"]])){
    if("nodeAlpha" %in% names(nodes) ){
      params[["alpha"]] <- nodes[["nodeAlpha"]]
    }
  }
  
  #--- edges
  
  if(is.null(params[["edge_colour"]])){
    if("edgeLineColor" %in% names(edges) ){
      params[["edge_colour"]] <- edges[["edgeLineColor"]]
    }
  }
  
  if(is.null(params[["edge_linewidth"]])){
    if("edgeLineWidth" %in% names(edges) ){
      params[["edge_linewidth"]] <- edges[["edgeLineWidth"]]
    }
  }
  
  if(is.null(params[["edge_linetype"]])){
    if("edgeLineType" %in% names(edges) ){
      params[["edge_linetype"]] <- edges[["edgeLineType"]]
    }
  }
  
  if(is.null(params[["edge_alpha"]])){
    if("edgeAlpha" %in% names(edges) ){
      params[["edge_alpha"]] <- edges[["edgeAlpha"]]
    }
  }
  
  params
}


#-------------------------------------------------------------------------------
#' @title GeomGraphSpace: a ggplot2 prototype for GraphSpace-class methods
#'
#' @description
#' 
#' \code{GeomGraphSpace} is the underlying \link[ggplot2]{ggproto} object 
#' used by \link{geom_graphspace} to draw node and edge elements in a 
#' graph layout. 
#'
#' This geom is designed for network diagrams, where graph attributes 
#' are often already in their final form (e.g., hex colors).
#'
#' @section Aesthetics:
#'
#' \code{GeomGraphSpace} understands ggplot2's conventions for point-like geoms.
#' 
#' @seealso
#' \link{geom_graphspace}, \link[ggplot2]{geom_point}
#'
#' @export
GeomGraphSpace <- ggproto(
  
  "GeomGraphSpace", ggplot2::Geom, 
  
  required_aes = c("x", "y", "vertex"),
  
  non_missing_aes = c("size", "shape", "colour"),
  
  default_aes = aes(
    size = 5,
    shape = 21,
    colour = "grey20",
    fill = "#E5E5E5B3",
    stroke = 0.5,
    alpha = NA
  ),
  
  draw_panel = function(self, data, panel_params, coord, edges, 
    edge_colour = "grey80", edge_alpha = NA,
    edge_linewidth = 0.5, edge_linetype = "solid", 
    arrow_size = 1, arrow_offset = 0.01, 
    arrow_lineend = "butt", arrow_linejoin = "mitre", 
    na.rm = FALSE, .size_unit = "mm") {
    
    data$shape <- translate_shape_string(data$shape)
    
    data <- .geom_check_node_size(data, size_unit = .size_unit)
    
    coords <- coord$transform(data, panel_params)
    
    # Create node grobs
    node_grobs <- .get_node_grobs(coords, size_unit = .size_unit)
    node_grobs$name <- grobName(node_grobs, "nodes")
    
    if(.empty(edges)){
      
      edge_grobs <- zeroGrob()
      
    } else {
      
      edges$colour <- edge_colour %||% "grey80"
      edges$linewidth <- edge_linewidth %||% 0.5
      edges$linetype <- edge_linetype %||% "solid"
      
      edges$alpha <- edge_alpha %||% NA
      edges$arrow_size <- (arrow_size %||% 1)
      edges$arrow_offset <- arrow_offset %||% 0
      
      edges <- remove_missing(edges, na.rm = na.rm,
        vars = c("vertex1", "vertex2", "arrowType"), 
        name = "geom_graphspace-edges")
      
      edges <- .geom_remap_edge_coords(edges = edges, nodes = coords)
      
      edges <- .geom_get_edge_offsets(edges = edges, nodes = coords,
        size_unit = .size_unit)
      
      edges <- .geom_adj_arrow_offsets(edges)
      
      edges <- .geom_adj_arrow_size(edges, size_unit = .size_unit)
      
      edges <- remove_missing(edges, na.rm = na.rm,
        vars = c("x", "y", "xend", "yend"), 
        name = "geom_graphspace-coords")
      
      # Create edge grobs
      edge_grobs <- .get_edge_grobs(edges, lineend = arrow_lineend, 
        linejoin = arrow_linejoin, size_unit = .size_unit)
      
    }
    
    grid::gTree(
      children = grid::gList(edge_grobs, node_grobs),
      name = grid::grobName(prefix = "geom_graphspace")
    )
    
  },
  
  draw_key = draw_key_point
  
)

#-------------------------------------------------------------------------------
# Remap x, y, xend, and yend to updated coords
.geom_remap_edge_coords <- function(edges, nodes){
  
  if(.empty(edges)){
    return( edges )
  }
  
  # remap segments
  idx <- match(edges[["vertex1"]], nodes[["vertex"]])
  edges[["x"]] <- nodes[["x"]][idx]
  edges[["y"]] <- nodes[["y"]][idx]
  idx <- match(edges[["vertex2"]], nodes[["vertex"]])
  edges[["xend"]] <- nodes[["x"]][idx]
  edges[["yend"]] <- nodes[["y"]][idx]
  
  return(edges)
  
}

