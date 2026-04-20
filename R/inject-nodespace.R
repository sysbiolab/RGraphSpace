
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
  
  object$size_par <- list()
  object$size_aes <- NULL
  object$size_rule <- NULL
  object$size_unit <- NULL
  
  ##--- ATTEMPT AUTO-DETECTION OF MAPPINGS
  has_nodes <- FALSE
  has_edges <- FALSE
  is_compatible <- FALSE
  p_layers <- list()
  for (i in seq_along(plot@layers)){
    layer <- plot@layers[[i]]
    if (inherits(layer$geom, "GeomNodeSpace") && !has_nodes){
      has_nodes <- TRUE
      p_layers[["nodes"]] <- layer
    }
    if (inherits(layer$geom, "GeomEdgeSpace") && !has_edges){
      has_edges <- TRUE
      p_layers[["edges"]] <- layer
    }
    if(has_nodes && has_edges){
      atts <- c("vertex", "x", "y", "name")
      is_compatible <- identical(p_layers[["edges"]]$geom_params$.nodes[,atts],
        p_layers[["nodes"]]$data[,atts])
      break
    }
  }
  
  if(!is_compatible){
    warning("`inject_nodespace()` failed to detect a compatible layers.",
      call. = FALSE)
    return(plot)
  }
  
  has_aes <- FALSE
  layer <- p_layers[["nodes"]]
  if (inherits(layer$mapping, "uneval")) {
    # Extract aes for clipping calculations
    m <- layer$mapping[names(layer$mapping) %in% c("size", "stroke")]
    if (length(m) > 0) {
      has_aes <- TRUE
      object$size_aes <- m
    }
  }
  if(is.list(layer$aes_params)){
    p <- layer$aes_params[names(layer$aes_params) %in% c("size", "stroke")]
    if (length(p) > 0) object$size_par <- p
  }
  if(is.list(layer$geom_params)){
    u <- layer$geom_params[[".size_unit"]]
    if (!is.null(u)) object$size_unit <- u
  }

  ##--- PREPARE SCALES
  
  has_scales <- FALSE
  size_scales <- Filter(function(s) "size" %in% s$aesthetics, 
    plot$scales$scales)

  if (length(size_scales) > 0){
    sz <- size_scales[[1]] 
    if (inherits(sz, "Scale") && !is.null(sz$palette)){
      has_scales <- TRUE
      object$size_rule <- sz
    }
  }
  
  if(has_aes && !has_scales){
    warning("`inject_nodespace()` could not find a size scale; ",
      "clipping might be inaccurate.",
      call. = FALSE)
  }
  
  ##--- INJECTION INTO EDGE LAYERS
  
  for (i in seq_along(plot@layers)) {

    if (inherits(plot@layers[[i]]$geom, "GeomEdgeSpace")) {
      
      # Inject the scale rule if found and valid
      if (!is.null(object$size_rule)) {
        plot@layers[[i]]$geom_params$.size_rule <- object$size_rule
      }
      
      # Inject the node mappings (captured or manual)
      if (!is.null(object$size_aes)) {
        plot@layers[[i]]$geom_params$size_aes <- object$size_aes
      }
      
      # Inject the user-defined params if present
      if (!is.null(object$size_par$size)) {
          plot@layers[[i]]$geom_params$.nodes[["size"]] <- object$size_par$size
      }
      
      if (!is.null(object$size_par$stroke)) {
          plot@layers[[i]]$geom_params$.nodes[["stroke"]] <- object$size_par$stroke
      }
      
      if (!is.null(object$size_unit)) {
        plot@layers[[i]]$geom_params$.size_unit <- object$size_unit
      }
    }
  }
  
  return(plot)
  
}
