
#-------------------------------------------------------------------------------
#' @title Draw node elements in a 2D graph layout
#' 
#' @description
#' 
#' Constructor for \link{GeomNodeSpace} ggproto objects.
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
#' drawing function in \link{GeomNodeSpace}.
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
#' @return A ggplot2 layer that renders node glyphs defined by
#' \link{GeomNodeSpace}.
#'
#' @section Aesthetics:
#'
#' \code{geom_nodespace()} understands \link[ggplot2]{geom_point} aesthetics.
#' 
#' If these aesthetics are not explicitly provided in \code{aes()}, they 
#' are automatically retrieved from the \link{GraphSpace} object.
#'
#' \tabular{ll}{
#'   \strong{\code{x}, \code{y}} \tab Required (automatically supplied).\cr
#'   \code{fill} \tab Node interior colour (see \link[ggplot2]{aes_colour_fill_alpha}).\cr
#'   \code{colour} \tab Node border colour (see \link[ggplot2]{aes_colour_fill_alpha}).\cr
#'   \code{alpha} \tab Transparency (see \link[ggplot2]{aes_colour_fill_alpha}).\cr
#'   \code{shape} \tab Node shape (see \link{points} and \link[ggplot2]{aes_linetype_size_shape}).\cr
#'   \code{size} \tab Node size (see *drawing* section and \link[ggplot2]{aes_linetype_size_shape}).\cr
#'   \code{stroke} \tab Node line width (see \link[ggplot2]{gg_par} and \link[ggplot2]{aes_linetype_size_shape}).
#' }
#' 
#' Required aesthetics \code{x} and \code{y} are supplied from the 
#' \link{GraphSpace} object and do not need to be manually mapped.
#' 
#' Additional parameters can be passed to control fixed values for the layer.
#' For example: `fill = "red"`, `stroke = 3`, `alpha = 0.5`, or `shape = 21`.
#' 
#' @section Drawing:
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
#' @seealso
#' \link{geom_edgespace}, \link{GraphSpace}
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
#' geom_edgespace(data = gs, arrow_offset = 0.01) +
#'   geom_nodespace(mapping = aes(size = nodeSize, fill = user_var2), 
#'   data = gs) + 
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
#' geom_edgespace(data = gs, arrow_offset = 0.01) +
#' geom_nodespace(mapping = aes(fill = user_var2), data = gs) +
#' theme(aspect.ratio = 1)
#'   
#' }
#' 
#' @export
geom_nodespace <- function(mapping = NULL, data = NULL, 
  stat = "identity", position = "identity", ...,
  na.rm = FALSE, show.legend = NA, inherit.aes = FALSE) {
  
  if (inherits(data, "GraphSpace")) {
    .geom_check_slots(data)
    nodes <- gs_nodes(data)
  } else {
    stop("'data' must be a 'GraphSpace' object.")
  }
  
  mapping <- .mapping_nodespace(mapping)
  
  params <- rlang::list2(na.rm = na.rm, ...)
  
  params <- .params_nodespace(params, mapping, nodes)
  
  ggplot2::layer(
    geom = GeomNodeSpace,
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
.geom_check_slots <- function(data){
  
  nodes <- gs_nodes(data)
  edges <- gs_edges(data)
  
  essential_natt <- c("x", "y", "vertex")
  essential_eatt <- c("vertex1", "vertex2", "arrowType")
  
  missing_n <- essential_natt[!(essential_natt %in% names(nodes))]
  if (length(missing_n) > 0) {
    stop("Required columns missing in slot 'data@nodes': ", 
      paste(missing_n, collapse = ", "), call. = FALSE)
  }
  
  missing_e <- essential_eatt[!(essential_eatt %in% names(edges))]
  if (length(missing_e) > 0) {
    stop("Required columns missing in slot 'data@edges': ", 
      paste(missing_e, collapse = ", "), call. = FALSE)
  }
  
  if (any(is.na(nodes[, essential_natt, drop = FALSE]))) {
    stop("Slot 'data@nodes' contains NAs in essential geometry columns: ", 
      paste(essential_natt, collapse = ", "), call. = FALSE)
  }
  
  if (any(is.na(edges[, essential_eatt, drop = FALSE]))) {
    stop("Slot 'data@edges' contains NAs in essential geometry columns: ", 
      paste(essential_eatt, collapse = ", "), call. = FALSE)
  }
  
}

#-------------------------------------------------------------------------------
.mapping_nodespace <- function(mapping) {
  x <- y <- NULL
  default_mapping <- ggplot2::aes(x = x, y = y)
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- utils::modifyList(default_mapping, mapping)
  }
  return(mapping)
}

#-------------------------------------------------------------------------------
.params_nodespace <- function(params, mapping, nodes){
  
  params$.size_unit <- if("size" %in% names(mapping)) "mm" else "npc"
  
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
  
  params
}


#-------------------------------------------------------------------------------
#' @title GeomNodeSpace: a ggplot2 prototype for GraphSpace-class methods
#'
#' @description
#' 
#' \code{GeomNodeSpace} is the underlying \link[ggplot2]{ggproto} object 
#' used by \link{geom_nodespace} to draw node elements in a graph layout.
#'
#' This geom is designed for network diagrams, where graph attributes 
#' are often already in their final form (e.g., hex colors).
#'
#' @section Aesthetics:
#'
#' \code{GeomNodeSpace} understands ggplot2's conventions for point-like geoms.
#' 
#' @seealso
#' \link{geom_nodespace}, \link[ggplot2]{geom_point}
#'
#' @importFrom ggplot2 scale_colour_identity draw_key_point from_theme zeroGrob
#' @importFrom ggplot2 geom_point geom_segment aes Geom .pt geom_text gg_par
#' @importFrom ggplot2 element_rect margin element_blank layer theme_bw
#' @importFrom ggplot2 element_line element_text ggproto theme theme_gray
#' @importFrom ggplot2 scale_linetype_manual annotation_raster coord_fixed
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous expansion labs
#' @importFrom ggplot2 expansion translate_shape_string is_waiver 
#' @importFrom ggplot2 remove_missing
#' @importFrom grid gpar arrow unit pointsGrob segmentsGrob
#' @importFrom grid grobTree gList grobName
#' @importFrom scales alpha squish
#' @importFrom utils modifyList
#' @importFrom rlang list2 warn exec '%||%'
#' @export
GeomNodeSpace <- ggproto(
  
  "GeomNodeSpace", ggplot2::GeomPoint, 
  
  required_aes = c("x", "y"),
  
  non_missing_aes = c("size", "stroke", "shape", "colour"),
  
  default_aes = aes(
    size = 5,
    shape = 21,
    colour = "grey20",
    fill = "grey80",
    stroke = 0.5,
    alpha = NA
  ),
  
  draw_panel = function(self, data, panel_params, coord, 
    na.rm = FALSE, .size_unit = "mm") {
    
    data$shape <- translate_shape_string(data$shape)
    
    data <- .geom_check_node_size(data, .size_unit)
    
    coords <- coord$transform(data, panel_params)
    
    # Create node grobs
    grobs <- .get_node_grobs(coords, .size_unit)
    grobs$name  <- grobName(grobs, "geom_nodespace")
    grobs
    
  },
  
  draw_key = draw_key_point
  
)

#-------------------------------------------------------------------------------
.geom_check_node_size <- function(nodes, size_unit = "mm", 
  range = NULL, verbose = TRUE){
  # 1. Passed as a aesthetic, 'size' follows 'geom_point' behavior, in mm;
  # 2. Passed as a parameter, 'size' scales with the viewport (%) with
  # range expected in [0, 100], then converted to 'npc'.
  if (size_unit == "mm") {
    if(!is.null(range)){
      nodes$size <- scales::rescale(sqrt(nodes$size), to = range, 
        from = c(0, max(sqrt(nodes$size), na.rm = T)))
    }
  } else {
    if(verbose) .check_node_size(nodes[["size"]])
    nodes$size <- scales::squish(nodes[["size"]], range = c(0, 100))
  }
  return(nodes)
}

.check_node_size <- function(size) {
  if ( any(size < 0 | size > 100, na.rm = TRUE) ){
    rlang::warn(
      message = c(
        "Node 'size' outside the expected range of [0, 100].",
        "i" = "Passed as a parameter, 'size' scales with the viewport (%).",
        "*" = "Use aes(size = ...) for data-driven scaling."
      ),
      .frequency = "always",
      .frequency_id = "graphspace_node_size_range"
    )
  }
}

#-------------------------------------------------------------------------------
# pointsGrob
.get_node_grobs <- function(coords, size_unit){
  
  if(size_unit == "npc"){
    # For 'npc', 'size' is in [0, 100], here transformed by a 0.01 factor
    # For 'pch' in 0:25, 'size' is about 75% of the character height
    # (see 'points()' graphics); here rectified by a 1/0.75 factor
    coords$size <- coords$size * 0.01 * 1/0.75
  }
  
  grid::pointsGrob(
    x = coords$x,
    y = coords$y,
    pch = coords$shape,
    size = grid::unit(coords$size, size_unit),
    gp = ggplot2::gg_par(
      fill = scales::alpha(coords$fill, coords$alpha),
      col = scales::alpha(coords$colour, coords$alpha),
      stroke = coords$stroke, pointsize = coords$size
    )
  )
  
}

