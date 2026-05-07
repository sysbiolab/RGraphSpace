
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
#' 
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

#' @export
#' @method ggplot_add inject_nodespace
ggplot_add.inject_nodespace <- function(object, plot, ...) {
  
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
  
  ##--- CHECK COMPATIBILITY BETWEEN LAYERS
  
  is_compatible <- FALSE
  
  if(has_nodes && has_edges){
    node_data <- p_layers$nodes$data
    edge_data <- p_layers$edges$data
    fl <- 1
    if(.is_handler(node_data, "node") && .is_handler(edge_data, "edge")){
      fl <- c(fl, 2)
      data <- .get_gs_graph(plot$data)
      if(inherits(data, "GraphSpace")){
        fl <- c(fl, 3)
        node_data <- gs_nodes(data)
        edge_data <- gs_edges(data)
      }
    } else if(identical(node_data, edge_data)){
      fl <- c(fl, 4)
      data <- .get_gs_graph(node_data)
      if(inherits(data, "GraphSpace")){
        fl <- c(fl, 5)
        node_data <- gs_nodes(data)
        edge_data <- gs_edges(data)
      }
    }
    if (!is.null(node_data) && !is.null(edge_data)){
      fl <- c(fl, 6)
      is_compatible <- .is_data_compatible(node_data, edge_data)
    }
  }
  
  # print(fl) #for debug only
  
  if(!is_compatible){
    warning("`inject_nodespace()` failed to detect compatible edge/node layers.",
      call. = FALSE)
    return(plot)
  }
  
  ##--- EXTRACT NODE DATA, AES, AND PARAMS
  
  p_pars <- list()
  p_pars$size_unit <- "mm"
  has_aes <- FALSE
  
  # Extract aes
  if (inherits(p_layers$nodes$mapping, "uneval")) {
    m <- p_layers$nodes$mapping[names(p_layers$nodes$mapping) %in% c("size", "stroke")]
    if (length(m) > 0) {
      has_aes <- TRUE
      p_pars$size_aes <- m
    }
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
  
  if(has_aes && !has_scales){
    warning("`inject_nodespace()` could not find a size scale; ",
      "clipping might be inaccurate.",
      call. = FALSE)
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
  
  ##--- INJECT NODE METADATA INTO EDGE LAYERS
  check_idx <- !is.null(p_layers$e_idx) && p_layers$e_idx <= length(plot@layers)
  has_clipping_cols <- any(c("size", "stroke") %in% colnames(node_data))
  if(check_idx && has_clipping_cols){
    plot@layers[[p_layers$e_idx]]$geom_params$.size_unit <- p_pars$size_unit
    plot@layers[[p_layers$e_idx]]$geom_params$.nodes <- node_data 
  } else {
    message("`inject_nodespace()` did not find edge clipping adjustments.")
  }
  
  return(plot)
  
}

#-------------------------------------------------------------------------------
.get_gs_graph <- function(data) {
  if (inherits(data, "GraphSpace")) {
    return(data)
  }
  if (inherits(data, "layout_ggraph")) {
    g <- attr(data, "graph")
    coords <- tryCatch(
      as.matrix(data[, c("x", "y")]),
      error = function(e) NULL
    )
    data <- GraphSpace(g, layout = coords, verbose = FALSE)
  } else if(inherits(data, "igraph")){
    data <- GraphSpace(data, verbose = FALSE)
  }
  return(data)
}

#-------------------------------------------------------------------------------
.is_data_compatible <- function(node_data, edge_data){
  
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
    message("`inject_nodespace()`: layers synchronized ", 
      "via vertex IDs (UUID mismatch).")
  }
  
  return( is_topo_ok )
  
}

#-------------------------------------------------------------------------------
.is_handler <- function(f, type = "node") {
  if (!is.function(f)) return(FALSE)
  inner_f <- environment(f)$data
  if ( is.null(inner_f) ) return(FALSE)
  b1 <- identical(attr(inner_f, "gs_handler_type"), type)
  b2 <- inherits(inner_f, paste0("gs_", type, "_handler"))
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
  if (size_unit == "npc") {
    node_size <- scales::squish(node_size, range = c(0, 100))
  } else {
    node_size <- scales::rescale(node_size, range = c(0, 1))
    node_size <- scales::rescale(sqrt(node_size), to = range)
  }
  return(node_size)
}

#-------------------------------------------------------------------------------
.geom_map_size_aes <- function(nodes, size_aes){
  
  required_att <- c("x", "y", "vertex", "size", "stroke")
  eval_att <- c(size = "nodeSize", stroke = "nodeLineWidth")
  default_att <- .get_default_vatt()
  
  if (!is.null(size_aes) && !inherits(size_aes, "uneval")) {
    size_aes <- NULL
    # warning("Argument 'size_aes' must be a mapping created with aes().", 
    #   call. = FALSE)
  }
  
  for(att in names(eval_att)){
    if (.is_valid_aes(att, size_aes, nodes)) {
      nodes[[att]] <- rlang::eval_tidy(size_aes[[att]], nodes)
    } else {
      if( !(att %in% colnames(nodes)) ){
        nodes[[att]] <- nodes[[eval_att[att]]] %||% default_att[[eval_att[att]]]
      }
      if(!is.numeric(nodes[[att]])){
        nodes[[att]] <- default_att[[eval_att[att]]]
      }
    }
  }
  nodes <- nodes[, intersect(required_att, colnames(nodes)), drop = FALSE]
  
  return(nodes)
  
}
.is_valid_aes <- function(att, mapping, data_df) {
  if (is.null(mapping)) return(FALSE)
  expr <- mapping[[att]]
  if (is.null(expr)) return(FALSE)
  vars <- all.vars(expr)
  if (length(vars) != 1) return(FALSE)
  vars %in% names(data_df)
}
