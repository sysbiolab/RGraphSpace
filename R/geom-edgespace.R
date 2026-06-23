
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
#' geometry (see 'drawing' section).
#' 
#' @param arrow_offset Numeric value controlling the base offset of arrows  
#' at edge endpoints (see 'drawing' section).
#'
#' @param lineend Line end style (round, butt, square). Supplied for 
#' compatibility with \link[ggplot2]{geom_segment}.
#' 
#' @param linejoin Line join style (round, mitre, bevel). Supplied for 
#' compatibility with \link[ggplot2]{geom_segment}.
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
#'   \strong{\code{x}, \code{y}, \code{xend}, \code{yend}} \tab Required (automatically supplied).\cr
#'   \code{colour} \tab Edge colour (see \link[ggplot2]{aes_colour_fill_alpha}).\cr
#'   \code{alpha} \tab Transparency (see \link[ggplot2]{aes_colour_fill_alpha}).\cr
#'   \code{linetype} \tab Edge line type (see \link[ggplot2]{aes_linetype_size_shape}).\cr
#'   \code{linewidth} \tab Edge line width (see \link[ggplot2]{aes_linetype_size_shape}).
#' }
#' 
#' Required aesthetics (\code{x}, \code{y}, \code{xend}, \code{yend}, ...)  
#' are supplied from the \link{GraphSpace} object and do not need to be 
#' manually mapped.
#' 
#' Additional parameters can be passed to control fixed values for the layer.
#' For example: `colour = "grey"`, `linetype = 2`, `linewidth = 1`.
#' 
#' Arrows can be further adjusted by \code{arrow_size} and \code{arrow_offset} 
#' arguments (see *details*).
#' 
#' @section Integration with ggraph:
#' 
#' \code{geom_edgespace} is compatible with the \code{ggraph} methods.
#' When used within a \code{ggraph()} call, the default \code{edgespace_handler()} 
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
#' **arrow_size** is a numeric scaling factor controlling arrowhead geometry. 
#' The value is interpreted in the same numeric space as line width (`lwd`), 
#' ensuring consistent scaling between edge strokes and arrowheads.
#' 
#' **arrow_offset** is an additive term that offsets arrow endpoints 
#' uniformly in graph space and is bounded by the edge length, in NPC units.
#' 
#' Arrowhead types are specified in the \link{GraphSpace} constructor.
#' 
#' @seealso
#' \link{GraphSpace}, \link{geom_nodespace}, \link{geom_graphspace}, 
#' \link[ggplot2]{geom_segment}
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
#' ggplot() +
#'   geom_edgespace(data = gs) +
#'   geom_nodespace(data = gs) +
#'   theme(aspect.ratio = 1)
#' 
#' }
#' 
#' @export
geom_edgespace <- function(mapping = NULL, data = NULL,
  stat = StatEdgeSpace, position = "identity", ..., 
  na.rm = FALSE, show.legend = NA, inherit.aes = FALSE,
  arrow_size = 1, arrow_offset = 0.01,
  lineend = "butt", linejoin = "mitre",
  raster = FALSE, dpi = NULL, dev = "cairo", scale = 1) {
  
  # Validate package-specific arguments;
  # All other arguments are validated elsewhere.
  .validate_gs_args("numeric_vec", "arrow_size", arrow_size)
  .validate_gs_args("numeric_vec", "arrow_offset", arrow_offset)
  
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
  optional_aes = c("edgeLineColor", "edgeLineWidth", "edgeLineType", "edgeAlpha"),
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
      data <- gs_edges(GraphSpace(data, verbose = FALSE))
    } else if (inherits(data, "GraphSpace")){
      data <- gs_edges(data)
    } else if (inherits(data, "gs_nodes")){
      if(inherits(attr(data, ".gs_graph"), "GraphSpace")){
        data <- gs_edges(attr(data, ".gs_graph"))
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
  
  x <- y <- xend <- yend <- arrowType <- NULL
  
  offset_start <- offset_end <- vertex1 <- vertex2 <- NULL
  
  edgeLineColor <- edgeLineWidth <- edgeLineType <- edgeAlpha <- NULL
  
  default_mapping <- ggplot2::aes(
    x = x, y = y, xend = xend, yend = yend,
    offset_start = offset_start,
    offset_end = offset_end,
    arrowType = arrowType, 
    vertex1 = vertex1, vertex2 = vertex2
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
  
  required_aes = c("x", "y", "xend", "yend", "arrowType",
    "vertex1", "vertex2", "offset_start", "offset_end"),
  
  non_missing_aes = c("linewidth", "linetype", "colour"),
  
  default_aes = ggplot2::aes(
    linewidth = 0.5,
    linetype = "solid",
    colour = "grey80",
    alpha = NA
  ),
  
  draw_panel = function(self, data, panel_params, coord,   
    arrow_size = 1, arrow_offset = 0.01, lineend = "butt", 
    linejoin = "mitre", na.rm = FALSE, raster = FALSE, 
    dpi = NULL, dev = "cairo", scale = 1, .size_unit = "mm", 
    .nodes = NULL) {
    
    required_att <- c("x", "y", "vertex", "size", "stroke")
    if(!is.null(.nodes) && all(required_att %in% colnames(.nodes))){
      data <- .geom_remap_edge_offsets(data, .nodes, size_unit = .size_unit)
    } else {
      data <- .geom_adj_edge_offsets(data, size_unit = .size_unit)
    }
    
    data$arrow_size <- (arrow_size %||% 1)
    data$arrow_offset <- arrow_offset %||% 0
    
    coords <- coord$transform(data, panel_params)
    
    coords <- .geom_adj_arrow_offsets(coords)
    
    coords <- .geom_adj_arrow_size(coords, size_unit = .size_unit)
    
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
    
    edge_grob
    
  },
  draw_key = draw_key_path
)

################################################################################
### segmentsGrob
################################################################################
.get_edge_grobs <- function(edges, lineend = "butt", 
  linejoin = "mitre", size_unit = "npc"){
  
  if(.empty(edges)){
    return( zeroGrob() )
  }
  
  edges$colour <- scales::alpha(edges$colour, edges$alpha)
  
  edges <- .set_arrows(edges, size_unit)
  
  arrows <- .get_arrows(edges, size_unit)
  
  grobs <- list()
  
  if (nrow(edges)>0) {
    gr <- grid::segmentsGrob(
      x0 = edges$x,
      y0 = edges$y,
      x1 = edges$xend,
      y1 = edges$yend,
      gp = ggplot2::gg_par(
        col = edges$colour,
        lwd = edges$linewidth, lty = edges$linetype,
        lineend = lineend, linejoin = linejoin
      )
    )
    gr$name <- grobName(gr, "edges")
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
  e_offsets <-  edges[["linewidth"]] * .lwd_offset_estimate(sz2npc)
  
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

#-------------------------------------------------------------------------------
.geom_adj_arrow_offsets <- function(edges){
  edges$offset_start <- edges[["offset_start"]] + edges[["arrow_offset"]]
  edges$offset_end <- edges[["offset_end"]] + edges[["arrow_offset"]]
  return(edges)
}

#-------------------------------------------------------------------------------
.geom_adj_arrow_size <- function(edges, size_unit){
  if(size_unit == "mm"){
    # ggplot2 'size' in 'mm', scaled to 'npc'
    edges$arrow_size <- edges[["arrow_size"]] * ggplot2::.pt
  } else {
    # gspace 'size' in [0, 100], transformed to 'npc'
    edges$arrow_size <- edges[["arrow_size"]] * ggplot2::.pt * 
      .gs_nsz_to_npc()
  }
  return(edges)
}

################################################################################
### Adjust arrows
################################################################################
.set_arrows <- function(edges, size_unit){
  edges <- .add_arrow_angle(edges)
  edges <- .adjust_arrow_size(edges, size_unit)
  edges <- .adjust_arrow_position(edges)
  return(edges)
}
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
.adjust_arrow_size <- function(edges, size_unit){
  edges$arrowSize1 <- edges[["arrow_size"]]
  edges$arrowSize2 <- edges[["arrow_size"]]
  if(size_unit=="npc"){
    # gspace 'size' in [0, 100], transformed to 'npc'
    lw <- edges$linewidth * .gs_nsz_to_npc()
  } else {
    lw <- edges$linewidth
  }
  a_theta <- 60 # default arrowhead opening angle;
  # grid::arrow() expects the half-angle
  a_theta <- a_theta / 180 * pi
  idx <- edges$arrowAngleStart==90
  if(any(idx, na.rm = TRUE)){
    l <- edges$arrowSize1[idx]/2
    b <- sqrt( (l^2 + l^2) - (2 * l^2) * cos(a_theta))
    edges$arrowSize1[idx] <- b + lw[idx]/4
  }
  idx <- edges$arrowAngleEnd==90
  if(any(idx, na.rm = TRUE)){
    l <- edges$arrowSize2[idx]/2
    b <- sqrt( (l^2 + l^2) - (2 * l^2) * cos(a_theta))
    edges$arrowSize2[idx] <- b + lw[idx]/4
  }
  return(edges)
}
.adjust_arrow_position <- function(edges){
  
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
  
  edges$px <- dx/L
  edges$py <- dy/L
  edges$x <- edges$x + (edges$px * offset_start)
  edges$y <- edges$y + (edges$py * offset_start)
  edges$xend <- edges$xend - (edges$px * offset_end)
  edges$yend <- edges$yend - (edges$py * offset_end)
  
  return(edges)
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
  exy$xend <- exy$x + (edges$px * 0.01)
  exy$yend <- exy$y + (edges$py * 0.01)
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
  exy$x <- exy$xend - (edges$px * 0.01)
  exy$y <- exy$yend - (edges$py * 0.01)
  # grid::arrow(length = ...) is vectorized; it has been tested and validated
  arrow <- grid::arrow(angle = edges$arrowAngleEnd,
    type = "open", ends = "last",
    length = grid::unit(edges$arrowSize2, size_unit))
  return(list(exy=exy, arrow = arrow, color = edges$colour, 
    linewidth = edges$linewidth))
}


