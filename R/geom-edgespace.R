
#-------------------------------------------------------------------------------
#' @title Draw edge elements in a 2D graph layout
#' 
#' @description
#' 
#' Constructor for \link{GeomEdgeSpace} ggproto objects.
#' 
#' A wrapper around \link[ggplot2]{geom_segment} that enables direct use of
#' edge attributes stored in \link{GraphSpace} objects as aesthetics.
#'
#' This \code{geom} is designed to create edge-level aesthetics such as
#' \code{colour} and \code{linewidth}, or any custom aesthetics defined 
#' in \link{GeomEdgeSpace}.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' These mappings override global aesthetics and are not inherited 
#' from the top-level plot.
#'
#' @param data The data to be displayed in this layer. It can be a 
#' \link{GraphSpace} object, an \link[igraph]{igraph} object, or the 
#' \code{edgespace_handler()} closure. When \code{NULL} (default),
#' a handler is created internally.
#'
#' @param stat The statistical transformation to use on the data.
#' Defaults to \code{identity}.
#'
#' @param position Position adjustment, either as a string or
#' the result of a call to a position adjustment function.
#'
#' @param ... Additional parameters passed to the underlying
#' drawing function in \link{GeomEdgeSpace}.
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
#' geometry (see 'details').
#' 
#' @param arrow_offset Numeric value controlling the base offset of arrows  
#' at edge endpoints (see 'details').
#'
#' @param curve Numeric. Controls edge curvature, as a fraction of edge
#' length. Non-zero values bow the edge into a smooth curve, and the sign 
#' controls which side it bows toward. Ignored for loops and parallel edges
#' (see 'details').
#'
#' @param parallel_spread Controls the lateral spread of parallel edges and 
#' self-loops. Ignored for simple non-loop edges (see 'details').
#' 
#' @param loop_direction Controls how self-loops are oriented around their
#' node. Options: \code{'adaptive'} (default), \code{'opposite'}, and an
#' angle in degrees (see 'details').
#' 
#' @param lineend Line end style (\code{'round'}, \code{'butt'}, 
#' \code{'square'}). Supplied for compatibility with \link[ggplot2]{geom_segment}.
#' 
#' @param linejoin Line join style (\code{'round'}, \code{'mitre'}, 
#' \code{'bevel'}). Supplied for compatibility with \link[ggplot2]{geom_segment}.
#' 
#' @param raster Logical. Should node glyphs be rasterized? 
#' Rasterization support is based on \code{\link[ggrastr]{rasterise}}.
#' 
#' @param dpi Numeric. Rasterization resolution.
#' 
#' @param dev Character. Rasterization backend. One of \code{'cairo'},
#' \code{'ragg'}, \code{'ragg_png'}, or \code{'cairo_png'}.
#' 
#' @param scale Numeric. Rasterization scaling factor
#' (see \code{\link[ggrastr]{rasterise}}).
#' 
#' @return A ggplot2 layer that renders edge segments defined by
#' \link{GeomEdgeSpace}.
#' 
#' @section Aesthetics:
#' 
#' \code{geom_edgespace()} understands \link[ggplot2]{geom_segment} aesthetics.
#' 
#' If these aesthetics are not explicitly provided in \code{aes()}, they 
#' are automatically retrieved from the \link{GraphSpace} object.
#'
#' \tabular{ll}{
#'   \strong{\code{x}, \code{y}, \code{xend}, \code{yend}} \tab Required; automatically supplied.\cr
#'   \code{colour} \tab Edge colour (see \link[ggplot2]{aes_colour_fill_alpha}).\cr
#'   \code{alpha} \tab Transparency (see \link[ggplot2]{aes_colour_fill_alpha}).\cr
#'   \code{linetype} \tab Edge line type (see \link[ggplot2]{aes_linetype_size_shape}).\cr
#'   \code{linewidth} \tab Edge line width (see \link[ggplot2]{aes_linetype_size_shape}).
#' }
#' 
#' Required aesthetics (\code{x}, \code{y}, \code{xend}, \code{yend})  
#' are supplied from the \link{GraphSpace} object and do not need to be 
#' manually mapped.
#' 
#' Additional parameters can be passed to control fixed values for the layer.
#' For example: `colour = "grey"`, `linetype = 2`, `linewidth = 1`.
#' 
#' Arrows can be further adjusted by \code{arrow_size} and \code{arrow_offset} 
#' arguments (see *details*).
#' 
#' @section Label aesthetics:
#' 
#' When \code{label} is mapped via \code{aes()}, a text label is drawn at
#' the visual midpoint of each edge. Labels follow the rendered edge geometry:
#' the chord midpoint for straight edges, the Bezier midpoint for curved edges,
#' and the loop apex for self-loops. Edges with \code{NA} labels are silently
#' skipped.
#' 
#' The \code{label_colour} aesthetic defaults to the edge \code{colour}, and
#' \code{label_alpha} defaults to the edge \code{alpha}. All other
#' \code{label_*} aesthetics default to \code{\link[ggplot2]{geom_label}}
#' when not set.
#' 
#' \tabular{ll}{
#'   \strong{\code{label}} \tab Required to activate label rendering.\cr
#'   \code{label_colour}   \tab Label text colour (see \code{\link[ggplot2]{geom_label}}).\cr
#'   \code{label_alpha}    \tab Transparency (see \code{\link[ggplot2]{geom_label}}).\cr
#'   \code{label_fill}     \tab Background colour (see \code{\link[ggplot2]{geom_label}}).\cr
#'   \code{label_size}     \tab Font size (see \code{\link[ggplot2]{geom_label}}).\cr
#'   \code{label_angle}    \tab Rotation angle (see \code{\link[ggplot2]{geom_label}}).\cr
#'   \code{label_hjust}    \tab Horizontal justification (see \code{\link[ggplot2]{geom_label}}).\cr
#'   \code{label_vjust}    \tab Vertical justification (see \code{\link[ggplot2]{geom_label}}).\cr
#'   \code{label_lwd}      \tab Border linewidth (see \code{\link[ggplot2]{geom_label}}).\cr
#'   \code{label_lty}      \tab Border linetype (see \code{\link[ggplot2]{geom_label}}).\cr
#'   \code{label_family}   \tab Font family (see \code{\link[ggplot2]{geom_label}}).\cr
#'   \code{label_fontface} \tab Font face (see \code{\link[ggplot2]{geom_label}}).\cr
#'   \code{label_lineheight} \tab Line height (see \code{\link[ggplot2]{geom_label}}).
#' }
#' 
#' @details
#' 
#' **arrow_size** is a numeric scaling factor controlling arrowhead geometry. 
#' The value is interpreted in the same numeric space as line width (`lwd`).
#' 
#' **arrow_offset** is an additive term that offsets arrow endpoints 
#' uniformly in graph space and is bounded by the edge length, in NPC units.
#' 
#' Arrowhead types are specified in the \link{GraphSpace} constructor.
#' 
#' **curve** bows an edge through a control point displaced perpendicular
#' to the edge, by \code{curve} times the edge length. \code{curve = 0}
#' (default) renders a straight edge. Typical visible values range from
#' about 0.1 to 0.4; sign sets which side the edge bows toward.
#' 
#' **parallel_spread** controls the fan opening for parallel edges,
#' reciprocal \code{A->B}/\code{B->A} pairs, and self-loops -- anything
#' where multiple edges share the same vertex pair. \code{curve} has no
#' effect on these edges; \code{parallel_spread} governs both their
#' curvature magnitude and how far apart they fan. A value of \code{0}
#' collapses all edges in a group onto the same position; increasing
#' values progressively open the fan. Self-loops behave the same way:
#' a single loop uses \code{parallel_spread} to set its own size, and
#' multiple loops at the same node fan out accordingly. A built-in
#' minimum, tied to \code{arrow_size} and node size, keeps small
#' \code{parallel_spread} values from producing a loop whose arrowhead
#' looks skewed against its own curvature.
#' 
#' **loop_direction** determines where self-loops sit relative to their node.
#' \code{"adaptive"} (default) points each loop in the direction that faces
#' away from the graph's centroid. \code{"opposite"} is a two-sided 
#' arrangement: loops are split into two groups placed above and below the 
#' node. A numeric angle (in degrees) places all loops at a fixed direction 
#' regardless of their node's position in the layout. When node position data 
#' is unavailable, \code{"adaptive"} silently falls back to \code{"opposite"}.
#' 
#' @seealso
#' \link{GraphSpace}, \link{geom_nodespace}, \link{geom_graphspace}, 
#' \link[ggplot2]{geom_segment}, \link[ggplot2]{geom_label}
#'
#' @examples
#' library(RGraphSpace)
#' library(igraph)
#' library(ggplot2)
#' 
#' # Load a demo igraph
#' data('gtoy1', package = 'RGraphSpace')
#' 
#' # Create a GraphSpace object
#' gs <- GraphSpace(gtoy1)
#' 
#' \dontrun{
#' 
#' ggplot(gs) +
#'   geom_edgespace() +
#'   geom_nodespace() +
#'   theme(aspect.ratio = 1)
#' 
#' }
#' 
#' @export
geom_edgespace <- function(mapping = NULL, data = NULL,
  stat = StatEdgeSpace, position = "identity", ..., 
  na.rm = FALSE, show.legend = NA, inherit.aes = FALSE,
  arrow_size = 0.5, arrow_offset = 0.01, curve = 0, 
  parallel_spread = 1, loop_direction = "adaptive", 
  lineend = "butt", linejoin = "mitre",
  raster = FALSE, dpi = NULL, dev = "cairo", scale = 1) {
  
  # Validate package-specific arguments;
  # All other arguments are validated elsewhere.
  .validate_gs_args("singleNumber", "arrow_size", arrow_size)
  .validate_gs_args("singleNumber", "arrow_offset", arrow_offset)
  .validate_gs_args("singleNumber", "curve", curve)
  .validate_gs_args("singlePositiveNumber", "parallel_spread", parallel_spread)
  if(is.character(loop_direction)){
    loop_direction <- match.arg(loop_direction, 
      choices = c("adaptive", "opposite"))
  } else {
    .validate_gs_args("singleNumber", "loop_direction", loop_direction)
  }
  
  if (is.null(data)){
    data <- edgespace_handler()
  } else if (!inherits(data, "edgespace_handler")){
    if (is.function(data)){
      rlang::abort(
        message = c(
          "x" = "Invalid handler function provided to `data`.",
          "*" = "Use `edgespace_handler()` to create a compatible handler."
        )
      )
    }
    data <- edgespace_handler()(data)
  }
  
  mapping <- .mapping_edgespace(mapping)
  
  params <- list2(
    na.rm = na.rm, 
    arrow_size = arrow_size,
    arrow_offset = arrow_offset,
    curve = curve,
    parallel_spread = parallel_spread,
    loop_direction = loop_direction,
    lineend = lineend,
    linejoin = linejoin,
    .size_unit = "mm",
    .nodes = NULL, 
    raster = raster, 
    dpi = dpi, 
    dev = dev, 
    scale = scale,
    ...)
  
  ggplot2::layer(
    geom = GeomEdgeSpace,
    stat = stat,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
  
}

#-------------------------------------------------------------------------------
#' Attribute Processing for GeomEdgeSpace
#'
#' Manage visual attribute precedence (color, size, shape) for `GeomEdgeSpace` 
#' objects.
#'
#' @section Attribute Priority:
#' 1. Explicit `aes()` mappings.
#' 2. Fixed `geom_edgespace()` arguments.
#' 3. Original graph attributes (via `optional_aes`).
#' 
#' During the `setup_data` stage, the Stat invokes internal functions 
#' to resolve value priority:
#' \enumerate{
#'   \item **Explicit Mapping**: Values defined by the user inside `aes()`.
#'   \item **Fixed Parameters**: Constant values passed as arguments in the `geom_edgespace()` call.
#'   \item **Graph Attributes**: Original attributes stored within the GraphSpace 
#'   object, retrieved from the data columns.
#' }
#'
#' @format A \code{ggproto} object.
#' @seealso \code{\link{geom_edgespace}}
#' @export
StatEdgeSpace <- ggproto(
  "StatEdgeSpace", ggplot2::Stat,
  optional_aes = c("edgeLineColor", "edgeLineWidth", 
    "edgeLineType", "edgeAlpha", "curve_weight", 
    "away_angle", "is_multiple", "is_loop"),
  setup_data = function(data, params) {
    data <- .params_edgespace(params, data)
    return(data)
  },
  compute_panel = function(data, scales){
    return(data)
  }
)

#-------------------------------------------------------------------------------
#' @rdname geom_edgespace
#' @export
edgespace_handler <- function() {
  
  fn <- function(data) {
    
    if (is_waiver(data)) return(NULL)
    
    if ( inherits(data, c("igraph", "layout_ggraph")) ) {
      data <- gs_edges(GraphSpace(data, verbose = FALSE), render = TRUE)
    } else if (inherits(data, "GraphSpace")){
      data <- gs_edges(data, render = TRUE)
    } else if (inherits(data, "gs_nodes")){
      if(inherits(attr(data, ".gs_graph"), "GraphSpace")){
        data <- gs_edges(attr(data, ".gs_graph"), render = TRUE)
      } else {
        rlang::warn(
          message = c(
            "x" = "`edgespace_handler()` found no edges in the input data.",
            "i" = "Input must be a 'GraphSpace', 'igraph', 'tbl_graph', or 'layout_ggraph'."
          )
        )
        data <- NULL
      }
    } else if (!inherits(data, "gs_edges")){
      rlang::abort(
        message = c(
          "x" = "`edgespace_handler()` received an unsupported object type.",
          "i" = "Input must be a 'GraphSpace', 'igraph', 'tbl_graph', or 'layout_ggraph'."
        )
      )
    }
    
    return(data)
    
  }
  
  attr(fn, "gs_handler_type") <- "edge"
  
  class(fn) <- c("edgespace_handler", class(fn))
  
  return(fn)
  
}

#-------------------------------------------------------------------------------
.mapping_edgespace <- function(mapping) {
  
  x <- y <- xend <- yend <- vertex1 <- vertex2 <- arrowType <- NULL
  
  offset_start <- offset_end <- curve_weight <- away_angle <- is_multiple <- is_loop <- NULL
  
  edgeLineColor <- edgeLineWidth <- edgeLineType <- edgeAlpha <- NULL
  
  default_mapping <- ggplot2::aes(
    x = x, y = y, xend = xend, yend = yend,
    vertex1 = vertex1, vertex2 = vertex2,
    arrowType = arrowType, 
    offset_start = offset_start,
    offset_end = offset_end,
    curve_weight = curve_weight,
    away_angle = away_angle,
    is_multiple = is_multiple,
    is_loop = is_loop
  )
  
  optional_mapping <- ggplot2::aes(
    edgeLineColor = edgeLineColor, 
    edgeLineWidth = edgeLineWidth,
    edgeLineType = edgeLineType,
    edgeAlpha = edgeAlpha)
  
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
.params_edgespace <- function(params, edges, mapping){
  
  # Note: 'mapping' is read from colnames(edges) because, at this
  # stage, ggplot2 has already evaluated aes() and pruned unmapped
  # columns. Any standard aesthetic name present here (e.g. "colour") 
  # was explicitly mapped by the user.
  if(missing(mapping)){
    mapping <- colnames(edges)
  } else {
    mapping <- names(mapping)
  }
  
  if(is.null(params[["colour"]]) &&  !"colour" %in% mapping){
    if("edgeLineColor" %in% names(edges) ){
      edges[["colour"]] <- edges[["edgeLineColor"]]
    }
  }
  
  if(is.null(params[["linewidth"]]) && !"linewidth" %in% mapping ){
    if("edgeLineWidth" %in% names(edges) ){
      edges[["linewidth"]] <- edges[["edgeLineWidth"]]
    }
  }
  
  if(is.null(params[["linetype"]]) && !"linetype" %in% mapping ){
    if("edgeLineType" %in% names(edges) ){
      edges[["linetype"]] <- edges[["edgeLineType"]]
    }
  }
  
  if(is.null(params[["alpha"]]) && !"alpha" %in% mapping ){
    if("edgeAlpha" %in% names(edges) ){
      edges[["alpha"]] <- edges[["edgeAlpha"]]
    }
  }
  
  edges$curve_weight <- edges$curve_weight %||% 1
  edges$is_multiple  <- edges$is_multiple  %||% FALSE
  edges$is_loop      <- edges$is_loop      %||% FALSE
  edges$away_angle   <- edges$away_angle   %||% NA_real_
  
  return(edges)
  
}

#-------------------------------------------------------------------------------
#' @title GeomEdgeSpace: a ggplot2 prototype for GraphSpace-class methods
#'
#' @description
#' 
#' \code{GeomEdgeSpace} is the underlying \link[ggplot2]{ggproto} object 
#' used by \link{geom_edgespace} to draw edge elements in a graph layout. 
#'
#' This geom is designed for network diagrams, where graph attributes 
#' are often already in their final form (e.g., hex colors).
#' 
#' @section Aesthetics:
#'
#' \code{GeomEdgeSpace} understands ggplot2's conventions for segment-like geoms.
#' 
#' @seealso
#' \link{geom_edgespace}, \link[ggplot2]{geom_segment}
#'
#' @importFrom ggplot2 draw_key_path
#' @export
GeomEdgeSpace <- ggproto(
  
  "GeomEdgeSpace", ggplot2::GeomSegment,
  
  required_aes = c(
    "x", "y", "xend", "yend", 
    "vertex1", "vertex2", "arrowType", 
    "offset_start", "offset_end"),
  
  optional_aes = c("label", "label_size", "label_colour", 
    "label_fill", "label_alpha","label_angle", "label_hjust", 
    "label_vjust", "label_lwd", "label_lty", "label_family",
    "label_fontface", "label_lineheight"),
  
  non_missing_aes = c("linewidth", "linetype", "colour"),
  
  default_aes = ggplot2::aes(
    linewidth = 0.5,
    linetype = "solid",
    colour = "grey80",
    alpha = NA
  ),
  
  draw_panel = function(self, data, panel_params, coord,   
    arrow_size = 0.5, arrow_offset = 0.01, curve = 0, 
    parallel_spread = 1, loop_direction = "adaptive", lineend = "butt", 
    linejoin = "mitre", na.rm = FALSE, raster = FALSE, 
    dpi = NULL, dev = "cairo", scale = 1, .size_unit = "mm", 
    .nodes = NULL) {
    
    required_att <- c("x", "y", "vertex", "size", "stroke")
    if(!is.null(.nodes) && all(required_att %in% colnames(.nodes))){
      data <- .geom_remap_edge_offsets(data, .nodes, size_unit = .size_unit)
    } else {
      data <- .geom_adj_edge_offsets(data, size_unit = .size_unit)
    }
    
    # Edge attributes supplied by the geom
    arrow_size <- arrow_size %||% 1
    arrow_size[is.na(arrow_size)] <- 1
    data$arrow_size <- arrow_size
    arrow_offset <- arrow_offset %||% 0
    arrow_offset[is.na(arrow_offset)] <- 0
    data$arrow_offset <- arrow_offset
    
    # `curve` governs ordinary, non-competing edges; `parallel_spread`
    # governs anything that needs curve_weight's fan-out to remain
    # distinguishable -- parallel edges and self-loops
    curve <- curve %||% 0
    parallel_spread <- parallel_spread %||% 1
    loop_direction <- loop_direction %||% "adaptive"
    
    data <- .resolve_edge_curve(data, curve, parallel_spread)
    
    coords <- coord$transform(data, panel_params)
    
    coords <- .geom_set_arrows(coords, .size_unit, loop_direction)
    
    # Create edge grobs
    grobs <- .get_edge_grobs(coords, lineend = lineend, 
      linejoin = linejoin, size_unit = .size_unit)
    
    edge_grob <- grid::gTree(children = grobs,
      name = grid::grobName(prefix = "geom_edgespace")
    )
    
    if (raster) {
      edge_grob <- .as_rasteriser(edge_grob, dpi = dpi, 
        dev = dev, scale = scale)
    }
    
    # Create label grob
    if ("label" %in% colnames(coords) && !all(is.na(coords$label))) {
      label_grob <- .get_edge_label_grob(coords, coord, panel_params)
      edge_grob <- grid::gTree(children = grid::gList(edge_grob, label_grob))
    }
    
    edge_grob
    
  },
  draw_key = draw_key_path
)

################################################################################
### GeomLabel
################################################################################
.get_edge_label_grob <- function(coords, coord, panel_params){
  
  l_data <- coords[!is.na(coords$label), , drop = FALSE]
  if (nrow(l_data) == 0){
    return( zeroGrob() )
  }
  
  l_data <- .get_edge_label_xy(l_data, panel_params)
  
  l_data$colour <- l_data$label_colour %||% l_data$colour %||% "black"
  l_data$alpha <- l_data$label_alpha %||% l_data$alpha %||% NA_real_
  l_data$fill <- l_data$label_fill %||% "white"
  
  if (!is.null(l_data$label_size)) l_data$size <- l_data$label_size
  if (!is.null(l_data$label_angle)) l_data$angle <- l_data$label_angle
  if (!is.null(l_data$label_hjust)) l_data$hjust <- l_data$label_hjust
  if (!is.null(l_data$label_vjust)) l_data$vjust <- l_data$label_vjust
  if (!is.null(l_data$label_lwd)) l_data$linewidth <- l_data$label_lwd
  if (!is.null(l_data$label_lty)) l_data$linetype <- l_data$label_lty
  if (!is.null(l_data$label_family)) l_data$family <- l_data$label_family
  if (!is.null(l_data$label_fontface)) l_data$fontface <- l_data$label_fontface
  if (!is.null(l_data$label_lineheight)) l_data$lineheight <- l_data$label_lineheight
  
  l_data <- ggplot2::GeomLabel$use_defaults(l_data)
  
  ggplot2::GeomLabel$draw_panel(l_data, panel_params, coord)
  
}

.get_edge_label_xy <- function(edges, panel_params){
  
  is_loop <- edges$is_loop
  is_curved <- !is.na(edges$curve) & edges$curve != 0 & !is_loop
  is_straight <- !is_loop & !is_curved
  
  lx <- numeric(nrow(edges))
  ly <- numeric(nrow(edges))
  
  # Straight chord: chord midpoint
  if (any(is_straight)) {
    lx[is_straight] <- (edges$x[is_straight] + edges$xend[is_straight]) / 2
    ly[is_straight] <- (edges$y[is_straight] + edges$yend[is_straight]) / 2
  }
  
  # Curved chord: quadratic Bezier midpoint at t=0.5.
  # B(0.5) = (P0 + 2*P1 + P2)/4 -- cx/cy are now available in coords
  # after .geom_set_arrows() runs .adjust_arrow_position_chord().
  if (any(is_curved)) {
    e <- edges[is_curved, , drop = FALSE]
    lx[is_curved] <- (e$x + 2*e$cx + e$xend) / 4
    ly[is_curved] <- (e$y + 2*e$cy + e$yend) / 4
  }
  
  # Self-loop: cubic Bezier midpoint at t=0.5 using all four control
  # points. cx1/cy1/cx2/cy2 are available in coords after
  # .adjust_arrow_position_loop() runs. Using cx/cy (= cx1/cy1) alone
  # places the label 30 degrees off the true apex direction, causing
  # visible drift -- the full formula is both simple and exact.
  if (any(is_loop)) {
    e <- edges[is_loop, , drop = FALSE]
    lx[is_loop] <- 0.125*e$x + 0.375*e$cx1 + 0.375*e$cx2 + 0.125*e$xend
    ly[is_loop] <- 0.125*e$y + 0.375*e$cy1 + 0.375*e$cy2 + 0.125*e$yend
  }
  
  # Convert label positions from NPC space back to data space so that
  # GeomLabel$draw_panel() can apply coord$transform() correctly.
  r0x <- panel_params$x$rescale(panel_params$x$limits[1])
  r1x <- panel_params$x$rescale(panel_params$x$limits[2])
  r0y <- panel_params$y$rescale(panel_params$y$limits[1])
  r1y <- panel_params$y$rescale(panel_params$y$limits[2])
  lx<- (lx - r0x) / (r1x - r0x) *
    diff(panel_params$x$limits) + panel_params$x$limits[1]
  ly <- (ly - r0y) / (r1y - r0y) *
    diff(panel_params$y$limits) + panel_params$y$limits[1]
  
  edges$x <- lx
  edges$y <- ly
  
  return(edges)
  
}

################################################################################
### segmentsGrob
################################################################################
.get_edge_grobs <- function(edges, lineend = "butt", 
  linejoin = "mitre", size_unit = "npc"){
  
  if(.empty(edges)){
    return( zeroGrob() )
  }
  
  edges$colour <- scales::alpha(edges$colour, edges$alpha)
  
  arrows <- .get_arrows(edges, size_unit)
  
  grobs <- list()
  
  is_curved <- !is.na(edges$curve) & edges$curve != 0 & !edges$is_loop
  
  if (any(!is_curved & !edges$is_loop)) {
    straight <- edges[!is_curved & !edges$is_loop, , drop = FALSE]
    gr <- grid::segmentsGrob(
      x0 = straight$x,
      y0 = straight$y,
      x1 = straight$xend,
      y1 = straight$yend,
      gp = ggplot2::gg_par(
        col = straight$colour,
        lwd = straight$linewidth, lty = straight$linetype,
        lineend = lineend, linejoin = linejoin
      )
    )
    gr$name <- grobName(gr, "edges")
    grobs[[length(grobs) + 1]] <- gr
  }
  
  if (any(is_curved)) {
    curved <- edges[is_curved, , drop = FALSE]
    gr <- .curve_grob(curved, lineend = lineend, linejoin = linejoin)
    gr$name <- grobName(gr, "curvededges")
    grobs[[length(grobs) + 1]] <- gr
  }
  
  if (any(edges$is_loop)) {
    loopy <- edges[edges$is_loop, , drop = FALSE]
    gr <- .loop_grob(loopy, lineend = lineend, linejoin = linejoin)
    gr$name <- grobName(gr, "loopedges")
    grobs[[length(grobs) + 1]] <- gr
  }
  
  if (!is.null(arrows$ends)) {
    gr <- grid::segmentsGrob(
      x0 = arrows$ends$exy$x,
      y0 = arrows$ends$exy$y,
      x1 = arrows$ends$exy$xend,
      y1 = arrows$ends$exy$yend,
      arrow = arrows$ends$arrow,
      gp = ggplot2::gg_par(
        col = arrows$ends$color,
        lwd = arrows$ends$linewidth, lty = "solid",
        lineend = lineend, linejoin = linejoin
      )
    )
    gr$name <- grobName(gr, "arrowends")
    grobs[[length(grobs) + 1]] <- gr
  }
  
  if (!is.null(arrows$starts)) {
    gr <- grid::segmentsGrob(
      x0 = arrows$starts$exy$x,
      y0 = arrows$starts$exy$y,
      x1 = arrows$starts$exy$xend,
      y1 = arrows$starts$exy$yend,
      arrow = arrows$starts$arrow,
      gp = ggplot2::gg_par(
        col = arrows$starts$color,
        lwd = arrows$starts$linewidth, lty = "solid",
        lineend = lineend, linejoin = linejoin
      )
    )
    gr$name <- grobName(gr, "arrowstarts")
    grobs[[length(grobs) + 1]] <- gr
  }
  
  do.call(grid::gList, grobs)
  
}

#-------------------------------------------------------------------------------
# All self-loops are drawn here, regardless of `curve` value -- unlike
# ordinary edges, a loop has no straight-line fallback, so it always
# renders via this Bezier path. `curve` controls only the bow
# direction/magnitude; a floor in .adjust_arrow_position_loop()
# ensures a sensible minimum size even at curve == 0.
.loop_grob <- function(edges, lineend = "butt", linejoin = "mitre", n = 24){
  
  t <- seq(0, 1, length.out = n)
  omt <- 1 - t
  
  # cubic Bezier: B(t) = (1-t)^3 P0 + 3(1-t)^2 t C0 + 3(1-t)t^2 C1 + t^3 P1
  bx <- outer(omt^3, edges$x) +
    outer(3 * omt^2 * t, edges$cx1) +
    outer(3 * omt * t^2, edges$cx2) +
    outer(t^3, edges$xend)
  by <- outer(omt^3, edges$y) +
    outer(3 * omt^2 * t, edges$cy1) +
    outer(3 * omt * t^2, edges$cy2) +
    outer(t^3, edges$yend)
  
  grid::polylineGrob(
    x = as.vector(bx), y = as.vector(by),
    id = rep(seq_len(nrow(edges)), each = n),
    gp = ggplot2::gg_par(
      col = edges$colour, lwd = edges$linewidth, lty = edges$linetype,
      lineend = lineend, linejoin = linejoin
    )
  )
}

#-------------------------------------------------------------------------------
# Edges with `curve == 0` never reach this function; they are drawn as
# plain segments by `.get_edge_grobs()`
.curve_grob <- function(edges, lineend = "butt", linejoin = "mitre", n = 24){
  
  t <- seq(0, 1, length.out = n)
  one_minus_t <- 1 - t
  
  # quadratic Bezier: B(t) = (1-t)^2 * P0 + 2(1-t)t * P1 + t^2 * P2
  bx <- outer(one_minus_t^2, edges$x) +
    outer(2 * one_minus_t * t, edges$cx) +
    outer(t^2, edges$xend)
  by <- outer(one_minus_t^2, edges$y) +
    outer(2 * one_minus_t * t, edges$cy) +
    outer(t^2, edges$yend)
  
  grid::polylineGrob(
    x = as.vector(bx),
    y = as.vector(by),
    id = rep(seq_len(nrow(edges)), each = n),
    gp = ggplot2::gg_par(
      col = edges$colour,
      lwd = edges$linewidth, lty = edges$linetype,
      lineend = lineend, linejoin = linejoin
    )
  )
  
}

#-------------------------------------------------------------------------------
.empty <- function(df){
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is_waiver(df)
}

################################################################################
### Edge Offsets
################################################################################
# Here, the final node sizes computed by ggplot2 are available.
# Clipping offsets are adjusted using the effective node radius,
# stroke, and linewidth;
# The effective node radius is: (size / 2) + (stroke / 2); 'size' 
# represents the node diameter in points (mm * .pt) and is converted
# to 'npc' for alignment with grid coordinates;
# For stroke, see .stroke_offset_estimate();
# For linewidth, see .lwd_offset_estimate().
.geom_remap_edge_offsets <- function(edges, nodes, size_unit){
  
  # size-to-npc conversion factor (1 mm expressed in npc units)
  sz2npc <- grid::convertWidth(unit(1, "mm"), unitTo = "npc", valueOnly = T)
  
  if(size_unit=="mm"){
    # ggplot2 node 'size' in 'mm', scaled to 'npc'
    n_offsets <- nodes[["size"]]/2 * sz2npc
  } else {
    # gspace node 'size' in [0, 100], transformed to 'npc'
    n_offsets <- nodes[["size"]]/2 * .gs_nsz_to_npc()
  }
  # 'stroke' and 'linewidth' in 'mm', scaled to 'npc'
  n_offsets <- n_offsets + (nodes[["stroke"]] * .stroke_offset_estimate(sz2npc))
  e_offsets <- edges[["linewidth"]] * .lwd_offset_estimate(sz2npc)
  
  edges$offset_start <- n_offsets[edges[["vertex1"]]] + e_offsets
  edges$offset_end <- n_offsets[edges[["vertex2"]]] + e_offsets
  
  return(edges)
  
}

#-------------------------------------------------------------------------------
# Here, the final node sizes computed by ggplot2 are not available;
# Pre-computed clipping offsets are therefore adjusted using 
# 'size_unit', together with linewidth and a default stroke estimate
.geom_adj_edge_offsets <- function(edges, size_unit){
  
  # size-to-npc conversion factor (1 mm expressed in npc units)
  sz2npc <- grid::convertWidth(unit(1, "mm"), unitTo = "npc", valueOnly = T)
  
  if(size_unit=="mm"){
    # ggplot2 node 'size' in 'mm', scaled to 'npc'
    n_offsets <- sz2npc
  } else {
    # gspace node 'size' in [0, 100], transformed to 'npc'
    n_offsets <- .gs_nsz_to_npc()
  }
  stroke_offset <- .stroke_offset_estimate(sz2npc)
  lwd_offset <- edges[["linewidth"]] * .lwd_offset_estimate(sz2npc)
  
  edges$offset_start <- (edges[["offset_start"]]/2 * n_offsets) + 
    lwd_offset + stroke_offset
  edges$offset_end <- (edges[["offset_end"]]/2 * n_offsets) + 
    lwd_offset + stroke_offset
  
  return(edges)
  
}

#-------------------------------------------------------------------------------
# Estimate the stroke thickness added to the node radius.
# 1. 'stroke' is approximately 0.75 mm per unit (see 'aes_linetype_size_shape') 
#     and is pre-processed by gg_par() as (stroke * .stroke / 2).
# 2. Because the border is centered on the node boundary, only half of the
#    stroke extends outward. This correction estimates the effective increase
#    in node radius attributable to the stroke.
# sz2npc: size-to-npc conversion factor used to calculate node size
.stroke_offset_estimate <- function(sz2npc){
  0.5 * 0.75 * sz2npc
}
# Full linewidth contribution to the clipping offset
# Unlike 'stroke', no half-width correction is required.
.lwd_offset_estimate <- function(sz2npc){
  0.75 * sz2npc
}

################################################################################
### Resolve edge_curve: Simple edges bow away from the coordinate origin,
### with magnitude set by 'curve'. Parallel edges and self-loops fan out 
### independently with magnitude set by 'parallel_spread'.
################################################################################
.resolve_edge_curve <- function(data, curve, parallel_spread){
  
  loop_multi <- data$is_loop | data$is_multiple
  if(curve!=0){
    # Simple edges use 'curve' directly, scaled by 'curve_weight'
    # and oriented by 'outward_sign' (away from origin)
    outward_sign <- .resolve_outward_sign(data)
    data$curve[!loop_multi] <- curve * outward_sign[!loop_multi] *
      data$curve_weight[!loop_multi]
  } else {
    data$curve[!loop_multi] <- curve
  }
  
  # Log10 compresses 'parallel_spread' so the fan doesn't blow up
  data$curve[loop_multi] <- log10( parallel_spread + 1 ) * 
    data$curve_weight[loop_multi]
  
  data
  
}

# Derive origin-based away angle from midpoint coordinates
.resolve_outward_sign <- function(data){
  mid_x <- (data$x + data$xend) / 2
  mid_y <- (data$y + data$yend) / 2
  away_rad <- atan2(mid_y, mid_x)
  dx <- data$xend - data$x
  dy <- data$yend - data$y
  outward_sign <- sign(-dy * cos(away_rad) + dx * sin(away_rad))
  outward_sign[is.na(outward_sign)] <- 1
  outward_sign
}

################################################################################
### Adjust arrows
################################################################################
.geom_set_arrows <- function(edges, size_unit, loop_direction = "adaptive"){
  edges <- .adj_arrow_offset(edges)
  edges <- .add_arrow_angle(edges)
  edges <- .adj_arrow_size(edges, size_unit)
  edges <- .adj_arrow_position(edges, size_unit, loop_direction)
  return(edges)
}

#-------------------------------------------------------------------------------
.adj_arrow_offset <- function(edges){
  edges$offset_start <- edges[["offset_start"]] + edges[["arrow_offset"]]
  edges$offset_end <- edges[["offset_end"]] + edges[["arrow_offset"]]
  return(edges)
}

#-------------------------------------------------------------------------------
.add_arrow_angle <- function(edges){
  .a_start <- function(atype){
    a_angle <- rep(NA, length(atype))
    a_angle[atype %in% c(0, 1, -1)] <- 0
    a_angle[atype %in% c(2, 3, -4)] <- 30
    a_angle[atype %in% c(-2, -3, 4)] <- 90
    a_angle
  }
  .a_end <- function(atype){
    a_angle <- rep(NA, length(atype))
    a_angle[atype %in% c(0, 2, -2)] <- 0
    a_angle[atype %in% c(1, 3, 4)] <- 30
    a_angle[atype %in% c(-1, -3, -4)] <- 90
    a_angle
  }
  edges$arrowAngleStart <- .a_start(edges$arrowType)
  edges$arrowAngleEnd <- .a_end(edges$arrowType)
  return(edges)
}

#-------------------------------------------------------------------------------
.adj_arrow_size <- function(edges, size_unit){
  
  if(size_unit == "mm"){
    # ggplot2 'size' in 'mm', scaled to 'npc'
    edges$arrow_size <- edges[["arrow_size"]] * ggplot2::.pt
    lwidth <- edges$linewidth
  } else {
    # gspace 'size' in [0, 100], transformed to 'npc'
    edges$arrow_size <- edges[["arrow_size"]] * ggplot2::.pt * 
      .gs_nsz_to_npc()
    lwidth  <- edges$linewidth * .gs_nsz_to_npc()
  }
  edges$arrowSize1 <- edges[["arrow_size"]]
  edges$arrowSize2 <- edges[["arrow_size"]]
  a_theta <- 60 # default arrowhead opening angle;
  # grid::arrow() expects the half-angle
  a_theta <- a_theta / 180 * pi
  idx <- edges$arrowAngleStart==90
  if(any(idx, na.rm = TRUE)){
    l <- edges$arrowSize1[idx]/2
    b <- sqrt( (l^2 + l^2) - (2 * l^2) * cos(a_theta))
    edges$arrowSize1[idx] <- b + lwidth[idx]/4
  }
  idx <- edges$arrowAngleEnd==90
  if(any(idx, na.rm = TRUE)){
    l <- edges$arrowSize2[idx]/2
    b <- sqrt( (l^2 + l^2) - (2 * l^2) * cos(a_theta))
    edges$arrowSize2[idx] <- b + lwidth[idx]/4
  }
  return(edges)
}

#-------------------------------------------------------------------------------
.adj_arrow_position <- function(edges, size_unit, loop_direction = "adaptive"){
  
  # Pre-allocate every column either branch below writes, so both branches
  # return a data frame with an identical column set -- required for
  # `edges[mask, ] <- ...` row-subset assignment to line up correctly.
  edges$cx  <- NA_real_; edges$cy  <- NA_real_
  edges$px0 <- NA_real_; edges$py0 <- NA_real_
  edges$px1 <- NA_real_; edges$py1 <- NA_real_
  edges$cx1 <- NA_real_; edges$cy1 <- NA_real_
  edges$cx2 <- NA_real_; edges$cy2 <- NA_real_
  
  if (any(!edges$is_loop)) {
    edges[!edges$is_loop, ] <- .adjust_arrow_position_chord(edges[!edges$is_loop, , 
      drop = FALSE])
  }
  if (any(edges$is_loop)) {
    edges[edges$is_loop, ] <- .adjust_arrow_position_loop(edges[edges$is_loop, , 
      drop = FALSE], size_unit, loop_direction)
  }
  
  return(edges)
}

#-------------------------------------------------------------------------------
.adjust_arrow_position_chord <- function(edges){
  
  emode <- .get_emode(edges$arrowType)
  
  offset_start <- edges$offset_start * as.numeric(emode %in% c(2, 3))
  offset_end <- edges$offset_end * as.numeric(emode %in% c(1, 3))
  total_offset <- (offset_start + offset_end)
  
  dx <- edges$xend - edges$x
  dy <- edges$yend - edges$y
  L <- sqrt( dx^2 + dy^2 )
  L <- ifelse(L == 0, 1e-6, L)
  
  edge_body_len <- pmax(0.02, L * 0.2)
  available_space <- L - edge_body_len
  
  excess <- pmax(0, total_offset - available_space)
  
  adj_start <- offset_start - (excess / 2)
  adj_end <- offset_end - (excess / 2)
  
  final_start <- pmax(0, adj_start + pmin(0, adj_end))
  final_end <- pmax(0, adj_end + pmin(0, adj_start))
  
  idx <- which(excess > 0 & offset_start > 0)
  offset_start[idx] <- final_start[idx]
  
  idx <- which(excess > 0 & offset_end > 0)
  offset_end[idx] <- final_end[idx]
  
  # Direction along which each endpoint is pulled inward. For straight
  # edges (curve == 0) this reduces exactly to the chord direction used
  # previously; for curved edges it follows the curve's local tangent at
  # each endpoint, so the visible gap hugs the curve rather than the chord.
  geo <- .curve_geometry(edges$x, edges$y, edges$xend, edges$yend, edges$curve)
  
  edges$x <- edges$x + (geo$px0 * offset_start)
  edges$y <- edges$y + (geo$py0 * offset_start)
  edges$xend <- edges$xend - (geo$px1 * offset_end)
  edges$yend <- edges$yend - (geo$py1 * offset_end)
  
  # Recompute the curve geometry from the offset-adjusted endpoints, so
  # curvature stays proportional to the visible edge rather than the
  # original, unclipped one. These final tangents are also used to orient
  # arrowheads, and the control point is reused to draw the curve itself.
  geo <- .curve_geometry(edges$x, edges$y, edges$xend, edges$yend, edges$curve)
  edges$cx <- geo$cx
  edges$cy <- geo$cy
  edges$px0 <- geo$px0
  edges$py0 <- geo$py0
  edges$px1 <- geo$px1
  edges$py1 <- geo$py1
  
  return(edges)
}

#-------------------------------------------------------------------------------
# Tuning surface for self-loop geometry (see `.adjust_arrow_position_loop()`).
.loop_pars <- list(
  anchor_span = 25 * pi / 180, # half-angle of the node-side "neck"
  size_scale = 3,              # loop bulge size, relative to node radius
  arrow_curvature_frac = 0.4,  # max fraction of curvature radius the arrowhead may occupy
  shape_frac = 1,              # min bulge depth, as a multiple of neck width
  arrow_stagger = 0.4          # extra arrow-floor margin per rank step within a side
)

# Self-loop geometry. `d` (bulge depth) is the largest of three floors:
#   - d_requested: what `curve` alone asks for.
#   - d_shape_min: keeps the bulge deep enough, relative to neck width,
#     to read as a teardrop rather than a shallow cap.
#   - d_arrow_min: keeps the local radius of curvature large enough that
#     a fixed-size arrowhead doesn't look skewed against it; staggered
#     by rank among same-vertex, same-side siblings so loops at close
#     but distinct ranks (e.g. several loops fanned at one vertex) don't
#     collapse to visually identical depths.
# Anchors sit at the node's clipping radius `r` when an end has an
# arrowhead (so the arrowhead stays visible), or at the node's center
# otherwise (covered by the node glyph, so the loop still reads as
# plugged into the node without depending on `r` matching the node's
# true rendered size). Control points always sit at radius `r + d`,
# unaffected by the anchor choice -- since center, anchor, and control
# point are always collinear, this keeps arrowhead orientation exactly
# radial either way, with no extra logic needed.
.adjust_arrow_position_loop <- function(loops, size_unit, loop_direction = "adaptive"){
  
  r <- pmax(loops$offset_start, loops$offset_end, 1e-6, na.rm = TRUE)
  
  is_single_cluster <- !identical(loop_direction, "opposite")
  
  if (is_single_cluster && identical(loop_direction, "adaptive")) {
    has_away_angle <- "away_angle" %in% colnames(loops)
    if (!has_away_angle) {
      is_single_cluster <- FALSE
    }
  }
  
  if (!is_single_cluster) {
    # "opposite": loops split into two antipodal clusters
    side_ref <- ifelse(loops$curve == 0, loops$curve_weight, loops$curve)
    theta0 <- ifelse(side_ref >= 0, pi / 2, -pi / 2)
    curve_effective <- loops$curve
    rank_key <- paste(loops$vertex1, sign(side_ref), sep = "_")
    rank_value <- abs(loops$curve_weight)
  } else {
    # Single-cluster layout ("adaptive" or a fixed numeric angle): every
    # loop at a shared vertex clusters around one angle
    if (identical(loop_direction, "adaptive")) {
      # Each vertex points its loops away from the graph's centroid
      away_angle <- loops[["away_angle"]]
      theta0 <- ifelse(is.na(away_angle), pi / 2, away_angle * pi / 180)
    } else {
      # Fixed direction: loop_direction is a single user-supplied angle
      theta0 <- loop_direction * pi / 180
    }
    edge_spread_recovered <- loops$curve / loops$curve_weight
    local_weight <- stats::ave(seq_along(loops$vertex1), loops$vertex1,
      FUN = function(idx) .fan_onesided(length(idx)))
    curve_effective <- edge_spread_recovered * local_weight
    rank_key <- loops$vertex1
    rank_value <- local_weight
  }
  
  d_requested <- abs(curve_effective) * r * .loop_pars$size_scale
  neck_width <- 2 * r * sin(.loop_pars$anchor_span)
  d_shape_min <- .loop_pars$shape_frac * neck_width * rank_value
  
  emode <- .get_emode(loops$arrowType)
  has_start_arrow <- emode %in% c(2, 3)
  has_end_arrow <- emode %in% c(1, 3)
  
  arrow_len <- pmax(
    ifelse(has_start_arrow, loops$arrowSize1, 0),
    ifelse(has_end_arrow, loops$arrowSize2, 0)
  )
  arrow_len <- grid::convertWidth(grid::unit(arrow_len, size_unit),
    unitTo = "npc", valueOnly = TRUE)
  sin2s <- sin(2 * .loop_pars$anchor_span)
  Rmin <- arrow_len / .loop_pars$arrow_curvature_frac
  B <- Rmin * sin2s
  d_arrow_min <- (B + sqrt(B^2 + 6 * B * r)) / 3
  
  rank_in_group <- stats::ave(rank_value, rank_key,
    FUN = function(w) rank(w, ties.method = "first"))
  d_arrow_min <- d_arrow_min * (1 + .loop_pars$arrow_stagger * (rank_in_group - 1))
  
  d <- d_requested + pmax(d_shape_min, d_arrow_min)
  
  a0 <- theta0 + .loop_pars$anchor_span
  a1 <- theta0 - .loop_pars$anchor_span
  
  cx0 <- loops$x; cy0 <- loops$y
  
  loops$x <- ifelse(has_start_arrow, cx0 + r * cos(a0), cx0)
  loops$y <- ifelse(has_start_arrow, cy0 + r * sin(a0), cy0)
  loops$xend <- ifelse(has_end_arrow, cx0 + r * cos(a1), cx0)
  loops$yend <- ifelse(has_end_arrow, cy0 + r * sin(a1), cy0)
  
  loops$cx1 <- cx0 + (r + d) * cos(a0)
  loops$cy1 <- cy0 + (r + d) * sin(a0)
  loops$cx2 <- cx0 + (r + d) * cos(a1)
  loops$cy2 <- cy0 + (r + d) * sin(a1)
  
  tx0 <- loops$cx1 - loops$x; ty0 <- loops$cy1 - loops$y
  L0 <- sqrt(tx0^2 + ty0^2); L0  <- ifelse(L0 == 0, 1e-6, L0)
  tx1 <- loops$xend - loops$cx2; ty1 <- loops$yend - loops$cy2
  L1 <- sqrt(tx1^2 + ty1^2); L1  <- ifelse(L1 == 0, 1e-6, L1)
  
  loops$px0 <- tx0 / L0; loops$py0 <- ty0 / L0
  loops$px1 <- tx1 / L1; loops$py1 <- ty1 / L1
  
  loops$cx <- loops$cx1
  loops$cy <- loops$cy1
  
  return(loops)
}

################################################################################
### Quadratic-Bezier geometry shared by curved and straight edges alike.
################################################################################
# Given endpoints (x,y)-(xend,yend) and a curvature fraction `curve`, returns
# the control point (cx, cy) -- displaced perpendicular to the chord by
# `curve * edge length` -- together with the curve's tangent unit vectors at
# its start (px0, py0) and end (px1, py1).
# When curve == 0, the control point sits exactly on the chord, and both
# tangents reduce exactly to the chord's own direction: this is what lets
# straight edges (the default) be handled by the same formula used for
# curved ones, with no behavioural change.
.curve_geometry <- function(x, y, xend, yend, curve){
  
  dx <- xend - x
  dy <- yend - y
  L <- sqrt(dx^2 + dy^2)
  L <- ifelse(L == 0, 1e-6, L)
  
  # perpendicular unit vector, rotated 90 degrees counter-clockwise
  # from the chord direction
  perp_x <- -dy / L
  perp_y <- dx / L
  
  cx <- (x + xend)/2 + curve * L * perp_x
  cy <- (y + yend)/2 + curve * L * perp_y
  
  tx0 <- cx - x;  ty0 <- cy - y
  L0 <- sqrt(tx0^2 + ty0^2); L0 <- ifelse(L0 == 0, 1e-6, L0)
  
  tx1 <- xend - cx;  ty1 <- yend - cy
  L1 <- sqrt(tx1^2 + ty1^2); L1 <- ifelse(L1 == 0, 1e-6, L1)
  
  list(
    cx = cx, cy = cy,
    px0 = tx0 / L0, py0 = ty0 / L0,
    px1 = tx1 / L1, py1 = ty1 / L1
  )
  
}

################################################################################
### Arrow constructor
################################################################################
.get_arrows <- function(edges, size_unit = "mm"){
  emode <- .get_emode(edges$arrowType)
  #--- add arrow starts
  idx <- emode==2 | emode==3
  if(any(idx)){
    starts <- .arrow_starts(edges[idx,], size_unit)
  } else {
    starts <- NULL
  }
  #--- add arrow ends
  idx <- emode==1 | emode==3
  if(any(idx)){
    ends <- .arrow_ends(edges[idx,], size_unit)
  } else {
    ends <- NULL
  }
  arrows <- list(ends = ends, starts = starts)
  return(arrows)
}
.arrow_starts <- function(edges, size_unit){
  exy <- edges[,c("x", "y", "xend", "yend")]
  # a tiny segment (0.01 npc) is used to anchor the arrowhead
  exy$xend <- exy$x + (edges$px0 * 0.01)
  exy$yend <- exy$y + (edges$py0 * 0.01)
  # grid::arrow(length = ...) is vectorized; it has been tested and validate
  arrow <- grid::arrow(angle = edges$arrowAngleStart,
    type = "open", ends = "first",
    length = grid::unit(edges$arrowSize1, size_unit))
  return(list(exy=exy, arrow = arrow, color = edges$colour, 
    linewidth = edges$linewidth))
}
.arrow_ends <- function(edges, size_unit){
  exy <- edges[,c("x", "y", "xend", "yend")]
  # a tiny segment (0.01 npc) is used to anchor the arrowhead
  exy$x <- exy$xend - (edges$px1 * 0.01)
  exy$y <- exy$yend - (edges$py1 * 0.01)
  # grid::arrow(length = ...) is vectorized; it has been tested and validated
  arrow <- grid::arrow(angle = edges$arrowAngleEnd,
    type = "open", ends = "last",
    length = grid::unit(edges$arrowSize2, size_unit))
  return(list(exy=exy, arrow = arrow, color = edges$colour, 
    linewidth = edges$linewidth))
}


