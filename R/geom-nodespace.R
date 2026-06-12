
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
#' @param data The data to be displayed in this layer. It can be a 
#' \link{GraphSpace} object, an \link[igraph]{igraph} object, or the 
#' \code{nodespace_handler()} closure. When \code{NULL} (default),
#' a handler is created internally from the \code{mapping} argument.
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
#' @param raster Logical. Should node glyphs be rasterized? 
#' Rasterization support is based on \code{\link[ggrastr]{rasterise}}.
#' 
#' @param dpi Numeric. Rasterization resolution.
#' 
#' @param dev Character. Rasterization backend. One of `"cairo"`,
#' `"ragg"`, `"ragg_png"`, or `"cairo_png"`.
#' 
#' @param scale Numeric. Rasterization scaling factor
#' (see \code{\link[ggrastr]{rasterise}}).
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
#' @section Integration with ggraph:
#' 
#' \code{geom_nodespace} is compatible with the \code{ggraph} methods.
#' When used within a \code{ggraph()} call, the default \code{nodespace_handler()} 
#' automatically:
#' \itemize{
#'   \item Identifies the current \code{layout_ggraph}.
#'   \item Extracts the \code{x} and \code{y} coordinates calculated by \code{ggraph}.
#'   \item Reconstructs a temporary \code{GraphSpace} object to inject spatial 
#'   metadata and user-chosen \code{ggraph} layout.
#' }
#' 
#' @details
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
#' \link{GraphSpace}, \link{geom_edgespace}, \link{geom_graphspace}, 
#' \link[ggplot2]{geom_point}
#'
#' @examples
#' library(RGraphSpace)
#' library(igraph)
#' library(ggplot2)
#' 
#' # Make a demo igraph
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
#' # ggplot2 default behavior: size is translated 
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
#' # inherited from the igraph object, it is 
#' # interpreted as a percentage of the plotting 
#' # area and translated to NPC units.
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
  stat = StatNodeSpace, position = "identity", ...,
  na.rm = FALSE, show.legend = NA, inherit.aes = FALSE,  
  raster = FALSE, dpi = NULL, dev = "cairo", scale = 1) {
  
  if (is.null(data)) {
    data <- nodespace_handler(mapping)
  } else if (!inherits(data, "nodespace_handler")){
    if (is.function(data)){
      rlang::abort(
        message = c(
          "x" = "Invalid handler function provided to `data`.",
          "*" = "Use `nodespace_handler()` to create a compatible handler."
        )
      )
    }
    data <- nodespace_handler(mapping)(data)
  }
  
  mapping <- .mapping_nodespace(mapping)
  
  params <- rlang::list2(
    na.rm = na.rm, 
    raster = raster, 
    dpi = dpi, 
    dev = dev, 
    scale = scale, 
    ...)
  
  params$.size_unit <- if("size" %in% names(mapping)) "mm" else "npc"
  
  ggplot2::layer(
    geom = GeomNodeSpace,
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
#' Attribute Processing for GeomNodeSpace
#'
#' Manage visual attribute precedence (color, size, shape) for `GeomNodeSpace` 
#' objects.
#'
#' @section Attribute Priority:
#' 1. Explicit `aes()` mappings.
#' 2. Fixed `geom_nodespace()` arguments.
#' 3. Original graph attributes (via `optional_aes`).
#' 
#' During the `setup_data` stage, the Stat invokes internal functions 
#' to resolve value priority:
#' \enumerate{
#'   \item **Explicit Mapping**: Values defined by the user inside `aes()`.
#'   \item **Fixed Parameters**: Constant values passed as arguments in the `geom_nodespace()` call.
#'   \item **Graph Attributes**: Original attributes stored within the GraphSpace 
#'   object, retrieved from the data columns.
#' }
#'
#' @format A \code{ggproto} object.
#' @seealso \code{\link{geom_nodespace}}
#' @export
StatNodeSpace <- ggproto(
  "StatNodeSpace", ggplot2::Stat,
  optional_aes = c("nodeSize", "nodeShape", "nodeLineWidth", 
    "nodeColor", "nodeLineColor", "nodeAlpha"),
  setup_data = function(data, params) {
    data <- .params_nodespace(params, data)
    return(data)
  },
  compute_panel = function(data, scales){
    return(data)
  }
)

#-------------------------------------------------------------------------------
#' @rdname geom_nodespace
#' @export
nodespace_handler <- function(mapping = NULL) {
  
  fn <- function(data) {
    if (is_waiver(data)) return(NULL)
    vars <- .detect_mapping_vars(mapping)
    if ( inherits(data, c("igraph", "layout_ggraph")) ) {
      data <- gs_nodes(GraphSpace(data, verbose = FALSE))
    } else if (inherits(data, "GraphSpace")){
      data <- gs_nodes(data, vars = vars)
    } else if (inherits(data, "gs_nodes")){
      graph <- attr(data, ".gs_graph")
      if(inherits(graph, "GraphSpace")){
        data <- gs_nodes(graph, vars = vars)
      }
    } else {
      rlang::abort(
        message = c(
          "x" = "`nodespace_handler()` received an unsupported object type.",
          "i" = "Input must be a 'GraphSpace', 'gs_nodes', 'igraph', 'tidygraph', or 'ggraph' layout."
        )
      )
    }
    
    return(data)
  }
  attr(fn, "gs_handler_type") <- "node"
  class(fn) <- c("nodespace_handler", class(fn))
  return(fn)
}

#-------------------------------------------------------------------------------
.detect_mapping_vars <- function(mapping){
  if(is.null(mapping)){
    return(mapping)
  }
  vars <- unique(unlist(
    lapply(mapping, function(x) {
      tryCatch(all.vars(x), error = function(e) character())
    })
  ))
  return(vars)
}

#-------------------------------------------------------------------------------
.mapping_nodespace <- function(mapping) {
  
  x <- y <- NULL
  default_mapping <- ggplot2::aes(x = x, y = y)
  
  nodeColor <- nodeSize <- nodeShape <- nodeLineWidth <- 
    nodeLineColor <- nodeAlpha <- NULL
  optional_mapping <- ggplot2::aes(
    nodeColor = nodeColor, 
    nodeSize = nodeSize,
    nodeShape = nodeShape,
    nodeLineWidth = nodeLineWidth,
    nodeLineColor = nodeLineColor,
    nodeAlpha = nodeAlpha)
  
  if (is.null(mapping)) {
    mapping <- utils::modifyList(
      default_mapping, optional_mapping)
  } else {
    mapping <- utils::modifyList(utils::modifyList(
      default_mapping, optional_mapping), mapping)
  }
  
  return(mapping)
  
}

#-------------------------------------------------------------------------------
.params_nodespace <- function(params, nodes, mapping){
  
  # Note: 'mapping' is read from colnames(nodes) because, at this
  # stage, ggplot2 has already evaluated aes() and pruned unmapped
  # columns. Any standard aesthetic name present here (e.g. "size",
  # "fill") was explicitly mapped by the user.
  if(missing(mapping)){
    mapping <- colnames(nodes)
  } else {
    mapping <- names(mapping)
  }
  
  if(is.null(params[["size"]]) && !"size" %in% mapping){
    if("nodeSize" %in% names(nodes) ){
      nodes[["size"]] <- nodes[["nodeSize"]]
    }
  }
  
  if(is.null(params[["stroke"]]) && !"stroke" %in% mapping ){
    if("nodeLineWidth" %in% names(nodes) ){
      nodes[["stroke"]] <- nodes[["nodeLineWidth"]]
    }
  }
  
  if(is.null(params[["shape"]]) && !"shape" %in% mapping ){
    if("nodeShape" %in% names(nodes) ){
      nodes[["shape"]] <- nodes[["nodeShape"]]
    }
  }
  
  if(is.null(params[["fill"]]) && !"fill" %in% mapping ){
    if("nodeColor" %in% names(nodes) ){
      nodes[["fill"]] <- nodes[["nodeColor"]]
    }
  }
  
  if(is.null(params[["colour"]]) && !"colour" %in% mapping ){
    if("nodeLineColor" %in% names(nodes) ){
      nodes[["colour"]] <- nodes[["nodeLineColor"]]
    }
  }
  
  if(is.null(params[["alpha"]]) && !"alpha" %in% mapping ){
    if("nodeAlpha" %in% names(nodes) ){
      nodes[["alpha"]] <- nodes[["nodeAlpha"]]
    }
  }
  
  return(nodes)
  
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
#' @importFrom ggplot2 scale_colour_identity zeroGrob
#' @importFrom ggplot2 geom_point geom_segment aes Geom .pt geom_text gg_par
#' @importFrom ggplot2 element_rect margin element_blank layer theme_bw
#' @importFrom ggplot2 element_line element_text ggproto theme theme_gray
#' @importFrom ggplot2 scale_linetype_manual annotation_raster coord_fixed
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous expansion labs
#' @importFrom ggplot2 expansion translate_shape_string is_waiver 
#' @importFrom ggplot2 remove_missing geom_blank draw_key_point
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
    na.rm = FALSE, .size_unit = "mm", raster = FALSE, 
    dpi = NULL, dev = "cairo", scale = 1) {
    
    data$shape <- translate_shape_string(data$shape)
    
    data <- .geom_check_node_size(data, .size_unit)
    
    coords <- coord$transform(data, panel_params)
    
    # Create node grobs
    grobs <- .get_node_grobs(coords, .size_unit)
    grobs$name  <- grobName(grobs, "geom_nodespace")
    
    if(raster){
      grobs <- .as_rasteriser(grobs, dpi = dpi, 
        dev = dev, scale = scale)
    }
    
    grobs
    
  },
  
  draw_key = draw_key_point
  
)

#-------------------------------------------------------------------------------
.geom_check_node_size <- function(nodes, size_unit = "npc"){
  # 1. Passed as a aesthetic, 'size' follows 'geom_point' behavior, in mm;
  # 2. Passed as a parameter, 'size' scales with the viewport (%) with
  # range expected in [0, 100], then later converted to 'npc'.
  if (size_unit == "npc") {
    .check_node_size(nodes[["size"]])
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
      )
    )
  }
}

#-------------------------------------------------------------------------------
# pointsGrob
.get_node_grobs <- function(coords, size_unit){
  
  if(size_unit == "npc"){
    coords$size <- coords$size * .gs_pch_to_npc()
    grob <- grid::pointsGrob(
      x = coords$x,
      y = coords$y,
      pch = coords$shape,
      size = grid::unit(coords$size, size_unit),
      gp = ggplot2::gg_par(
        fill = scales::alpha(coords$fill, coords$alpha),
        col = scales::alpha(coords$colour, coords$alpha),
        stroke = coords$stroke
      )
    )
  } else {
    grob <- grid::pointsGrob(
      x = coords$x,
      y = coords$y,
      pch = coords$shape,
      gp = ggplot2::gg_par(
        fill = scales::alpha(coords$fill, coords$alpha),
        col = scales::alpha(coords$colour, coords$alpha),
        pointsize = coords$size, stroke = coords$stroke
      )
    )
  }
  
  grob

}

#-------------------------------------------------------------------------------
# For gspace, node 'size' is defined on a [0, 100] scale and converted
# to NPC units using a 0.01 factor.
# For plotting symbols ('pch') in 0:25, the effective symbol diameter is
# approximately 75% of the character height (see graphics::points()); 
# a 1/0.75 correction is applied to recover the intended size.
.gs_pch_to_npc <- function(){
  .gs_nsz_to_npc() * (1/0.75)
}
.gs_nsz_to_npc <- function(){
  0.01
}

#-------------------------------------------------------------------------------
# Internal rasterization adapter
# Currently delegates to the ggrastr rasteriser mechanism.
# Isolated here so GraphSpace can switch to a native implementation
# without requiring changes to geoms.
.as_rasteriser <- function(grob, dpi = NULL, dev = "cairo", scale = 1){
  class(grob) <- unique( c("rasteriser", class(grob)) )
  grob$dpi <- dpi
  grob$dev <- dev
  grob$scale <- scale
  grob
}
