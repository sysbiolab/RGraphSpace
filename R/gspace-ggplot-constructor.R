
#-------------------------------------------------------------------------------
#' Using ggplot2 with GraphSpace objects
#' 
#' @description
#' 
#' GraphSpace objects can be used directly with \pkg{ggplot2}, allowing node 
#' attributes and high-dimensional feature data to be mapped through standard 
#' aesthetic mappings without manual data extraction.
#' This integration enables:
#' \itemize{
#'   \item Lazy evaluation of node attributes and feature data.
#'   \item Automatic synchronization of node metadata for standard ggplot2
#'   geoms such as \link[ggplot2]{geom_point}.
#'   \item Automatic propagation of node metadata required for edge clipping
#'   and arrow placement in GraphSpace-native geoms.
#' }
#' 
#' @param data A \link{GraphSpace} object.
#' @param mapping Set of aesthetic mappings created by \link[ggplot2]{aes}.
#' Passed to \link[ggplot2]{ggplot}.
#' @param ... Additional arguments passed to \link[ggplot2]{ggplot}.
#' 
#' @details
#' When a GraphSpace object is supplied to \code{ggplot()}, RGraphSpace 
#' extends the standard ggplot2 build process to automatically resolve 
#' GraphSpace variables and synchronize node metadata required for edge 
#' rendering.
#' 
#' When using \code{ggplot()}, neither \link{nodespace_handler} nor
#' \link{inject_nodespace} need to be called explicitly.
#' 
#' @return A \code{gspace_plot} object extending \link[ggplot2]{ggplot}.
#' 
#' @seealso
#' \link{GraphSpace}, \link{geom_nodespace}, \link{geom_edgespace},
#' \link{inject_nodespace}, \link{nodespace_handler}, \link{edgespace_handler}
#' 
#' @examples
#' library(RGraphSpace)
#' library(igraph)
#' library(ggplot2)
#' 
#' # Generate a toy star graph
#' gtoy1 <- make_star(15, mode = "out")
#' V(gtoy1)$my_node_var <- runif(vcount(gtoy1), 1, 20)
#' 
#' # Create a GraphSpace object
#' gs <- GraphSpace(gtoy1, layout = layout_in_circle(gtoy1))
#' 
#' \dontrun{
#'
#' # Example 1: Using RGraphSpace-native geoms
#' # Edge clipping metadata are injected automatically
#' ggplot(gs) +
#'   geom_edgespace(colour = "red") +
#'   geom_nodespace(aes(size = my_node_var), 
#'   fill = "steelblue", stroke = 2) +
#'   scale_size(range = c(2, 15))
#'
#' # Example 2: Mixing native and general geoms
#' # Note possible clipping mismatch when combining
#' # geom_edgespace() with generic ggplot2 node geoms.
#' # Since geom_point() does not expose the final rendered
#' # node radius to RGraphSpace, edge clipping is estimated
#' # from layer parameters and may not exactly match the
#' # displayed node geometry.
#' ggplot(gs) +
#'   geom_edgespace(colour = "red") +
#'   geom_point(aes(x, y, size = my_node_var), 
#'   fill = "steelblue", stroke = 2, shape = 21) +
#'   scale_size(range = c(2, 15))
#'
#' }
#' 
#' @name ggplot-GraphSpace
#' @rdname ggplot-GraphSpace
#' @importFrom ggplot2 ggplot
#' @method ggplot GraphSpace
#' @export
ggplot.GraphSpace <- function(data, mapping = NULL, ...) {

  p <- NextMethod()
  
  class(p) <- c("gspace_plot", class(p))
  
  p
  
}

#-------------------------------------------------------------------------------
#' @importFrom ggplot2 fortify
#' @export
fortify.GraphSpace <- function(model, data, ...) {
  res <- gs_nodes(model)
  attr(res, ".gs_graph") <- model
  class(res) <- c("gspace_data", class(res))
  return(res)
}

#-------------------------------------------------------------------------------
#' @importFrom ggplot2 ggplot_build
#' @method ggplot_build gspace_plot
#' @export
ggplot_build.gspace_plot <- function(plot, ...) {
  plot <- .inject_graphspace_handlers(plot)
  plot <- .inject_nodespace_internal(plot, verbose = FALSE) 
  NextMethod()
}

#-------------------------------------------------------------------------------
#' Dynamic Scale Injection for Edge Clipping
#'
#' @description
#' Utility function for **RGraphSpace** that enables edge layers to scan 
#' adjacent nodes and determine their dimensions. This information is used 
#' to compute arrow clipping offsets, preventing edge geometry from 
#' overlapping node symbols.
#' 
#' @param ... Additional parameters passed to other methods (currently ignored).
#'
#' @details
#' This function operates in two stages within the `ggplot2` workflow:
#' 
#' 1. **Capture:** It scans the `plot` layers for a \link{GeomNodeSpace} 
#' to extract both mapping variables (from `aes()`) and static parameters 
#' (specifically `size` and `stroke`).
#' 
#' 2. **Injection:** It locates \link{GeomEdgeSpace} layers and injects
#' scale rules, captured mappings, and fixed parameters into the geometry 
#' parameters.
#'    
#' This "lazy injection" calculates edge clipping based on the actual scales 
#' used by the nodes, even if scales are defined after the layers.
#' 
#' **Note:** `inject_nodespace()` must be called last in the `ggplot` chain to 
#' allow the function to correctly scan all previously added layers and scales.
#'
#' @return An object of class `inject_nodespace`, which interacts with the 
#' ggplot2 `+` operator.
#' 
#' @seealso
#' \link{geom_edgespace}, \link{geom_nodespace}
#' 
#' @examples
#' library(RGraphSpace)
#' library(igraph)
#' library(ggplot2)
#' 
#' # Generate a toy star graph
#' gtoy1 <- make_star(15, mode="out")
#'
#' # Set node and edge attributes
#' V(gtoy1)$my_node_var <- runif(vcount(gtoy1), 1, 20)
#' E(gtoy1)$my_edge_var <-  runif(ecount(gtoy1), 1, 20)
#'
#' # Create a GraphSpace object with a circular layout
#' gs <- GraphSpace(gtoy1, layout = layout_in_circle(gtoy1))
#'
#' \dontrun{
#' # Build the plot
#' # Note that inject_nodespace() is called at the end to
#' # synchronize node sizes with edge clipping.
#' ggplot() +
#'   geom_edgespace(aes(colour = my_edge_var), data = gs) +
#'   geom_nodespace(aes(size = my_node_var), data = gs) +
#'   scale_size(range = c(2, 15)) +
#'   inject_nodespace()
#' }
#' 
#' @importFrom ggplot2 ggplot_add
#' @rdname inject_nodespace
#' @export
inject_nodespace <- function(...) {
  structure(list(...), class = "inject_nodespace")
}

#' @method ggplot_add inject_nodespace
#' @export
ggplot_add.inject_nodespace <- function(object, plot, ...) {
  .inject_nodespace_internal(plot, verbose = TRUE)
}

#-------------------------------------------------------------------------------
.inject_graphspace_handlers <- function(plot){
  
  if(!inherits(plot@data, "gspace_data")){
    return(plot)
  }
  
  for(i in seq_along(plot@layers)){
    
    layer <- plot@layers[[i]]
    
    # Skip layers with explicit non-waiver data
    if(!inherits(layer$data, "waiver")){
      next
    }
    
    # For all other waiver-data layers (including general geoms),
    # inject a 'nodespace_handler' carrying the layer's own mapping
    if (inherits(layer$geom, "GeomEdgeSpace")) {
      layer$data <- edgespace_handler()
    } else {
      layer$data <- nodespace_handler(mapping = layer$mapping)
    }
    
    plot@layers[[i]] <- layer
    
  }
  
  plot
  
}

#-------------------------------------------------------------------------------
# Core injection logic - standalone internal function
.inject_nodespace_internal <- function(plot, verbose = TRUE) {
  
  ##--- ATTEMPT AUTO-DETECTION OF LAYERS
  
  p_layers <- list()
  has_nodes <- FALSE
  has_edges <- FALSE
  
  for (i in seq_along(plot@layers)){
    layer <- plot@layers[[i]]
    if (inherits(layer$geom, "GeomNodeSpace") && !has_nodes){
      has_nodes <- TRUE
      p_layers$nodes <- layer
      p_layers$n_idx <- i
    }
    if (inherits(layer$geom, "GeomEdgeSpace") && !has_edges){
      has_edges <- TRUE
      p_layers$edges <- layer
      p_layers$e_idx <- i
    }
  }
  
  if (!has_nodes || !has_edges) {
    return(plot)
  }
  
  ##--- CHECK COMPATIBILITY BETWEEN LAYERS
  
  node_data <- p_layers$nodes$data
  edge_data <- p_layers$edges$data
  if(.is_handler(node_data, "node") && .is_handler(edge_data, "edge")){
    node_data <- tryCatch(node_data(plot$data), error = function(e) NULL)
    edge_data <- tryCatch(edge_data(plot$data), error = function(e) NULL)
  } else if(identical(node_data, edge_data)){
    data <- .get_gs_graph(node_data)
    if(inherits(data, "GraphSpace")){
      node_data <- gs_nodes(data)
      edge_data <- gs_edges(data)
    }
  }
  
  if(!.is_data_compatible(node_data, edge_data)){
    rlang::warn(
      message = c(
        "!" = "`inject_nodespace()` failed to detect compatible edge/node layers.",
        "i" = "Both layers must originate from the same source graph.",
        "*" = "Check 'Interoperability' in the vignette for integration examples."
      )
    )
    return(plot)
  }
  
  ##--- EXTRACT NODE DATA, AES, AND PARAMS
  
  p_pars <- list(size_unit = "mm")
  has_aes <- FALSE
  
  # Extract aes
  if (inherits(p_layers$nodes$mapping, "uneval")) {
    m <- p_layers$nodes$mapping[names(p_layers$nodes$mapping) %in% c("size", "stroke")]
    if (length(m) > 0) {
      has_aes <- TRUE
      p_pars$size_aes <- m
    }
  }
  if(verbose && !has_aes){
    rlang::warn(
      message = c(
        "!" = "`inject_nodespace()` could not find a 'size' aesthetic.",
        "i" = "Fixed node 'size' scales with the viewport.",
        "*" = "Use aes(size = ...) for data-driven scaling."
      )
    )
  }
  # Extract params
  if(is.list(p_layers$nodes$aes_params)){
    p <- p_layers$nodes$aes_params[names(p_layers$nodes$aes_params) %in% c("size", "stroke")]
    if (length(p) > 0) p_pars$size_params <- p
  }
  if(is.list(p_layers$nodes$geom_params)){
    u <- p_layers$nodes$geom_params[[".size_unit"]]
    if (!is.null(u)) p_pars$size_unit <- u
  }
  
  ##--- MAP AES
  
  # Map 'size' and 'stroke' aesthetics if present
  if(!is.null(p_pars$size_aes)){
    node_data <- .geom_map_size_aes(node_data, size_aes = p_pars$size_aes)
  }
  
  ##--- PREPARE SCALES
  
  has_scales <- FALSE
  size_scales <- Filter(function(s) "size" %in% s$aesthetics, plot$scales$scales)
  if (length(size_scales) > 0){
    sz <- size_scales[[1]] 
    if (inherits(sz, "Scale") && !is.null(sz$palette)){
      has_scales <- TRUE
      p_pars$size_scales <- sz
    }
  }
  stroke_scales <- Filter(function(s) "stroke" %in% s$aesthetics, plot$scales$scales)
  if (length(stroke_scales) > 0){
    sz <- stroke_scales[[1]] 
    if (inherits(sz, "Scale") && !is.null(sz$palette)){
      has_scales <- TRUE
      p_pars$stroke_scales <- sz
    }
  }
  
  if(verbose && has_aes && !has_scales){
    rlang::warn(
      message = c(
        "!" = "`inject_nodespace()` could not find a 'size' scale.",
        "i" = "Clipping may be inaccurate without data-driven dimensions.",
        "*" = "Ensure a `scale_size()` is provided."
      )
    )
  }
  
  ##--- APPLY SCALES TO NODE DATA
  
  # adjust 'size' by user-defined scales if present
  if(!is.null(p_pars$size_scales$palette) && !is.null(node_data[["size"]])){
    size_range <- p_pars$size_scales$palette(c(0, 1))
    node_data$size <- .geom_node_to_offset(node_data[["size"]],
      range = size_range, size_unit = p_pars$size_unit)
  }
  if(!is.null(p_pars$stroke_scales$palette) && !is.null(node_data[["stroke"]])){
    stroke_range <- p_pars$stroke_scales$palette(c(0, 1))
    node_data$stroke <- .geom_node_to_offset(node_data[["stroke"]],
      range = stroke_range, size_unit = p_pars$size_unit)
  }
  # replace 'size' by user-defined params if present
  if (!is.null(p_pars$size_params$size)) {
    node_data$size <- p_pars$size_params$size
  }
  # replace 'stroke' by user-defined params if present
  if (!is.null(p_pars$size_params$stroke)) {
    node_data$stroke <- p_pars$size_params$stroke
  }
  
  # check if only one size params was included
  if( sum(c("size", "stroke") %in% colnames(node_data)) == 1){
    node_data <- .geom_map_size_params(node_data)
  }
  
  ##--- INJECT NODE METADATA INTO EDGE LAYERS
  required_att <- c("x", "y", "vertex", "size", "stroke")
  has_clipping_cols <- all(required_att %in% colnames(node_data))
  check_idx <- !is.null(p_layers$e_idx) && p_layers$e_idx <= length(plot@layers)
  has_unit <- !is.null(p_pars$size_unit)
  if (check_idx && (has_clipping_cols || has_unit)){
    if (has_clipping_cols){
      plot@layers[[p_layers$e_idx]]$geom_params$.nodes <- node_data 
    }
    if (has_unit){
      plot@layers[[p_layers$e_idx]]$geom_params$.size_unit <- p_pars$size_unit
    }
  } else {
    if(verbose){
      rlang::warn(
        message =  "`inject_nodespace()` found no clipping adjustments."
      )
    }
  }
  
  return(plot)
  
}

#-------------------------------------------------------------------------------
.get_gs_graph <- function(data) {
  if (inherits(data, "GraphSpace")) {
    return(data)
  }
  if (inherits(data, "gs_nodes")) {
    if(inherits(attr(data, ".gs_graph"), "GraphSpace")){
      data <- attr(data, ".gs_graph")
    }
    return(data)
  }
  if(inherits(data, c("igraph", "layout_ggraph"))){
    data <- GraphSpace(data, verbose = FALSE)
  }
  return(data)
}

#-------------------------------------------------------------------------------
.is_data_compatible <- function(node_data, edge_data){
  
  if(is.null(node_data) || is.null(edge_data)){
    return(FALSE)
  }
  
  # Check handlers
  if( !.was_handled(node_data, "node") || 
      !.was_handled(edge_data, "edge") ){
    return(FALSE)
  }
  
  # Quick check via UUID match
  n_id <- attr(node_data, "gs_id") 
  e_id <- attr(edge_data, "gs_id")
  if(.is_singleString(n_id) && .is_singleString(e_id)){
    if(n_id == e_id) return(TRUE)
  }
  
  # Compare data
  id_nodes <- node_data$vertex
  id_edges <- c(edge_data$vertex1, edge_data$vertex2)
  if (is.null(id_nodes) || is.null(id_edges)){
    return(FALSE)
  }
  id_nodes <- as.character(id_nodes)
  id_edges <- as.character(unique(id_edges))
  is_topo_ok <- all(id_edges %in% id_nodes) 
  if(is_topo_ok){
    rlang::message_cnd(
      message = c(
        "`inject_nodespace()`: layers synced via vertex IDs.",
        "i" = "Fallback triggered by UUID mismatch between edge and node data.",
        "*" = "Ensure graph objects remain identical across all plot layers."
      )
    )
  }
  
  return( is_topo_ok )
  
}

#-------------------------------------------------------------------------------
.is_handler <- function(f, type = "node") {
  if (!is.function(f)) return(FALSE)
  # Retrieve the original handler from inside ggplot2's data
  # wrapper; depends on ggplot2 storing it as 'data' in the
  # closure environment.
  inner_f <- tryCatch(environment(f)$data, error = function(e) NULL)
  if ( is.null(inner_f) ) return(FALSE)
  b1 <- identical(attr(inner_f, "gs_handler_type"), type)
  b2 <- inherits(inner_f, paste0(type, "space_handler"))
  return( b1 || b2 )
}

#-------------------------------------------------------------------------------
.was_handled <- function(dt, type = "node") {
  if (!is.data.frame(dt)) return(FALSE)
  return( identical(attr(dt, "gs_handler_type"), type) )
}

#-------------------------------------------------------------------------------
.geom_node_to_offset <- function(node_size, range = c(1, 6), size_unit = "npc"){
  # 1. Passed as a aesthetic, 'size' follows 'geom_point' behavior, in mm;
  # 2. Passed as a parameter, 'size' scales with the viewport (%) with
  # range expected in [0, 100], then later converted to 'npc'.
  if (is.null(node_size)) return(NULL)
  if (size_unit == "npc") {
    # Expect node_size already in [0,100] viewport percent
    node_size <- scales::squish(node_size, range = c(0, 100))
  } else {
    node_size <- scales::rescale(node_size, range = c(0, 1))
    node_size <- scales::rescale(sqrt(node_size), to = range)
  }
  return(node_size)
}

#-------------------------------------------------------------------------------
.geom_map_size_aes <- function(nodes, size_aes){
  
  required_clipping <- c("x", "y", "vertex", "size", "stroke")
  size_att <- c(size = "nodeSize", stroke = "nodeLineWidth")
  default_att <- .get_default_vatt()
  
  if (!is.null(size_aes) && !inherits(size_aes, "uneval")) {
    size_aes <- NULL
  }
  
  for(att in names(size_att)){
    if (.is_valid_aes(att, size_aes, nodes)) {
      nodes[[att]] <- tryCatch({
        rlang::eval_tidy(size_aes[[att]], nodes)
      }, error = function(e) {
        NULL
      })
    } else {
      if( !(att %in% colnames(nodes)) ){
        nodes[[att]] <- nodes[[size_att[att]]] %||% default_att[[size_att[att]]]
      }
    }
    if(is.null(nodes[[att]]) || !is.numeric(nodes[[att]])){
      nodes[[att]] <- default_att[[size_att[att]]]
    }
  }
  nodes <- nodes[, intersect(required_clipping, colnames(nodes)), drop = FALSE]
  
  return(nodes)
  
}
.is_valid_aes <- function(att, mapping, data_df) {
  if (is.null(mapping)) return(FALSE)
  expr <- mapping[[att]]
  if (is.null(expr)) return(FALSE)
  vars <- all.vars(expr)
  if (length(vars) == 0) return(TRUE)
  all(vars %in% names(data_df))
}

.geom_map_size_params <- function(nodes){
  
  required_clipping <- c("x", "y", "vertex", "size", "stroke")
  size_att <- c(size = "nodeSize", stroke = "nodeLineWidth")
  default_att <- .get_default_vatt()

  for(att in names(size_att)){
    if( !(att %in% colnames(nodes)) ){
      nodes[[att]] <- nodes[[size_att[att]]] %||% default_att[[size_att[att]]]
    }
    if(is.null(nodes[[att]]) || !is.numeric(nodes[[att]])){
      nodes[[att]] <- default_att[[size_att[att]]]
    }
  }
  nodes <- nodes[, intersect(required_clipping, colnames(nodes)), drop = FALSE]
  
  return(nodes)
  
}
