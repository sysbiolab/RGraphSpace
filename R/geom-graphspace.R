
#-------------------------------------------------------------------------------
#' @title Draw node and edge elements in a 2D graph layout
#'
#' @description
#'
#' \lifecycle{deprecated}
#'
#' Deprecated as of v1.4.2. Use
#' \code{\link{geom_edgespace}() + \link{geom_nodespace}()} instead. These 
#' geoms support all current features including node-edge synchronization, 
#' labels, multiple edges, and self-loops.
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes
#' See \code{\link{geom_edgespace}} and \code{\link{geom_nodespace}}.
#'
#' @param arrow_size,arrow_offset,curve,edge_spread,loop_direction
#' See \code{\link{geom_edgespace}}.
#'
#' @param raster,dpi,dev,scale
#' See \code{\link{geom_nodespace}}.
#'
#' @param ... Additional arguments passed to the underlying geoms.
#'
#' @return A ggplot2 layer.
#'
#' @note This function is deprecated. Replace \code{geom_graphspace(data = gs)}
#' with \code{geom_edgespace() + geom_nodespace()} in your \code{ggplot(gs)}
#' call.
#'
#' @seealso
#' \link{geom_nodespace}, \link{geom_edgespace}
#'
#' @export
geom_graphspace <- function(mapping = NULL, data, 
  stat = "identity", position = "identity", ...,
  na.rm = FALSE, show.legend = NA, inherit.aes = FALSE,
  arrow_size = 0.5, arrow_offset = 0.01, 
  curve = 0, edge_spread = 0.2, loop_direction = "adaptive",
  raster = FALSE, dpi = NULL, dev = "cairo", scale = 1) {
  
  lifecycle::deprecate_warn(
    when = "1.4.2",
    what = "geom_graphspace()",
    details = paste(
      "Use `geom_edgespace() + geom_nodespace()` instead.",
      "These geoms support all current features including labels,",
      "edge separation, and loop direction."
    )
  )
  
  # Validate package-specific arguments;
  # All other arguments are validated elsewhere.
  .validate_gs_args("singleNumber", "arrow_size", arrow_size)
  .validate_gs_args("singleNumber", "arrow_offset", arrow_offset)
  .validate_gs_args("singleNumber", "curve", curve)
  .validate_gs_args("singleNumber", "edge_spread", edge_spread)
  if(is.character(loop_direction)){
    loop_direction <- match.arg(loop_direction, 
      choices = c("adaptive", "opposite"))
  } else {
    .validate_gs_args("singleNumber", "loop_direction", loop_direction)
  }
  
  if (missing(data) || is.null(data)){
    rlang::warn(
      message = c(
        "!" = "`geom_graphspace()` ignored: explicit `data` object is required.",
        "i" = "Accepted: 'GraphSpace', 'igraph', 'tidygraph', or 'ggraph' layout.",
        "*" = "For inherited data, use `geom_nodespace()` and `geom_edgespace()` instead."
      )
    )
    return(ggplot2::geom_blank())
  }
  
  mapping <- .mapping_graphspace(mapping)
  
  params <- rlang::list2(
    na.rm = na.rm,
    arrow_size = arrow_size,
    arrow_offset = arrow_offset,
    curve = curve,
    edge_spread = edge_spread,
    loop_direction = loop_direction,
    raster = raster, 
    dpi = dpi, 
    dev = dev, 
    scale = scale,
    ...)
  
  data <- .graphspace_handler(data)
  edges <- gs_edges(data, render = TRUE)
  data <- gs_nodes(data, vars = .detect_mapping_vars(mapping), render = TRUE)
  params <- .params_graphspace(params, mapping, data, edges)
  
  ggplot2::layer(
    geom = GeomGraphSpace,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
  
}

#-------------------------------------------------------------------------------
.graphspace_handler <- function(data) {
  if ( inherits(data, c("igraph", "layout_ggraph")) ) {
    data <- GraphSpace(data, verbose = FALSE)
  } else if (!inherits(data, "GraphSpace")) {
    rlang::abort(
      message = c(
        "x" = "Unsupported `data` type in `geom_graphspace()`.",
        "i" = "Accepted: 'GraphSpace', 'igraph', 'tidygraph', or 'ggraph' layout.",
        "*" = "For inherited data, use `geom_nodespace()` and `geom_edgespace()` instead."
      )
    )
  }
  return(data)
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
  params$.edges <- edges
  
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
#' \lifecycle{deprecated}
#'
#' The underlying \link[ggplot2]{ggproto} object used by the deprecated
#' \link{geom_graphspace}. Use \link{GeomEdgeSpace} and
#' \link{GeomNodeSpace} instead.
#'
#' @seealso
#' \link{GeomEdgeSpace}, \link{GeomNodeSpace}
#'
#' @export
GeomGraphSpace <- ggproto(
  
  "GeomGraphSpace", ggplot2::Geom, 
  
  required_aes = c("x", "y", "vertex"),
    
  non_missing_aes = c("size", "stroke", "shape", "colour"),
  
  default_aes = aes(
    size = 5,
    stroke = 0.5,
    shape = 21,
    colour = "grey20",
    fill = "#E5E5E5B3",
    alpha = NA
  ),
  
  draw_panel = function(self, data, panel_params, coord, 
    edge_colour = "grey80", edge_alpha = NA, edge_linewidth = 0.5, 
    edge_linetype = "solid", arrow_size = 0.5, arrow_offset = 0.01, 
    curve = 0, edge_spread = 0.2, loop_direction =  "adaptive",
    arrow_lineend = "butt", arrow_linejoin = "mitre", na.rm = FALSE, 
    raster = FALSE, dpi = NULL, dev = "cairo", scale = 1, .size_unit = "mm", 
    .edges = NULL) {
    
    data$shape <- translate_shape_string(data$shape)
    
    data <- .geom_check_node_size(data, size_unit = .size_unit)
    
    coords <- coord$transform(data, panel_params)
    
    # Create node grobs
    node_grobs <- .get_node_grobs(coords, size_unit = .size_unit)
    node_grobs$name <- grobName(node_grobs, "nodes")
    
    if(.empty(.edges)){
      
      edge_grobs <- zeroGrob()
      
    } else {
      
      # Edge attributes that can be inherited from the graph
      .edges$colour <- edge_colour %||% "grey80"
      .edges$alpha <- edge_alpha %||% NA
      .edges$linewidth <- edge_linewidth %||% 0.5
      .edges$linetype <- edge_linetype %||% "solid"
      
      # Edge attributes supplied by the geom only
      
      arrow_size <- arrow_size %||% 1
      arrow_size[is.na(arrow_size)] <- 1
      .edges$arrow_size <- arrow_size
      arrow_offset <- arrow_offset %||% 0
      arrow_offset[is.na(arrow_offset)] <- 0
      .edges$arrow_offset <- arrow_offset

      curve <- curve %||% 0
      edge_spread <- edge_spread %||% 0
      loop_direction <- loop_direction %||% "adaptive"

      uses_separation <- .edges$is_loop | (.edges$is_multiple %||% FALSE)
      curve_source <- ifelse(uses_separation, edge_spread, curve)
      .edges$curve <- curve_source * (.edges$curve_weight %||% 1)
      
      # Remove missing values inherited from the graph
      .edges <- remove_missing(.edges, na.rm = na.rm,
        vars = c("vertex1", "vertex2", "arrowType", 
          "colour", "linewidth", "linetype"), 
        name = "geom_graphspace-edges")
      
      .edges <- .geom_remap_edge_coords(edges = .edges, nodes = coords)
      
      .edges <- .geom_remap_edge_offsets(edges = .edges, nodes = coords,
        size_unit = .size_unit)
      
      .edges <- .geom_set_arrows(.edges, .size_unit, loop_direction)
      
      .edges <- remove_missing(.edges, na.rm = na.rm,
        vars = c("x", "y", "xend", "yend"), 
        name = "geom_graphspace-coords")
      
      # Create edge grobs
      edge_grobs <- .get_edge_grobs(.edges, lineend = arrow_lineend, 
        linejoin = arrow_linejoin, size_unit = .size_unit)
      
    }
    
    graph_grob <- grid::gTree(
      children = grid::gList(edge_grobs, node_grobs),
      name = grid::grobName(prefix = "geom_graphspace")
    )
    
    if (raster) {
      graph_grob <- .as_rasteriser(graph_grob, 
        dpi = dpi, dev = dev, scale = scale)
    }
    
    graph_grob
    
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

