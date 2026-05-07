
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
#' \code{gs_edge_handler()} handler (default).
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
#'   \code{colour} \tab Node border colour (see \link[ggplot2]{aes_colour_fill_alpha}).\cr
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
#' \code{geom_nodespace} is compatible with the \code{ggraph} methods.
#' When used within a \code{ggraph()} call, the default \code{gs_edge_handler()} 
#' handler automatically:
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
geom_edgespace <- function(mapping = NULL, data = gs_edge_handler(),
  stat = "identity", position = "identity", ..., 
  na.rm = FALSE, show.legend = NA, inherit.aes = FALSE,
  arrow_size = 1, arrow_offset = 0.01,
  lineend = "butt", linejoin = "mitre") {
  
  # Check custom params
  .validate_gs_args("singleLogical", "na.rm", na.rm)
  .validate_gs_args("singleNumber", "arrow_size", arrow_size)
  .validate_gs_args("singleNumber", "arrow_offset", arrow_offset)
  .validate_gs_args("singleString", "lineend", lineend)
  .validate_gs_args("singleString", "linejoin", linejoin)
  
  mapping <- .mapping_edgespace(mapping)
  
  params <- list2(
    na.rm = na.rm, 
    arrow_size = arrow_size,
    arrow_offset = arrow_offset,
    lineend = lineend,
    linejoin = linejoin,
    .size_unit = "mm",
    .nodes = NULL,
    ...)
  
  if (!inherits(data, "gs_edge_handler")){
    if (inherits(data, c("GraphSpace", "igraph"))){
      if (inherits(data, "GraphSpace")) .geom_check_slots(data)
      gs_handler <- gs_edge_handler()
      data <- gs_handler(data)
      params <- .params_edgespace(params, mapping, data)
    } else {
      stop("'data' must be a 'GraphSpace' or 'igraph' object.", call. = FALSE)
    }
  }
  
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
#' @rdname geom_edgespace
#' @export
gs_edge_handler <- function() {
  fn <- function(data) {
    if (inherits(data, "layout_ggraph")) {
      g <- attr(data, "graph")
      coords <- tryCatch(
        as.matrix(data[, c("x", "y")]),
        error = function(e) NULL
      )
      data <- gs_edges(GraphSpace(g, layout = coords, verbose = FALSE))
    } else if (inherits(data, "igraph")) {
      data <- gs_edges(GraphSpace(data, verbose = FALSE))
    } else if (inherits(data, "GraphSpace")){
      data <- gs_edges(data)
    }
    return(data)
  }
  attr(fn, "gs_handler_type") <- "edge"
  class(fn) <- c("gs_edge_handler", class(fn))
  return(fn)
}

#-------------------------------------------------------------------------------
.mapping_edgespace <- function(mapping) {
  x <- y <- xend <- yend <- arrowType <- NULL
  offset_start <- offset_end <- vertex1 <- vertex2 <- NULL
  default_mapping <- ggplot2::aes(
    x = x, y = y, xend = xend, yend = yend,
    offset_start = offset_start,
    offset_end = offset_end,
    arrowType = arrowType, 
    vertex1 = vertex1, vertex2 = vertex2
  )
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- utils::modifyList(default_mapping, mapping)
  }
  return(mapping)
}

#-------------------------------------------------------------------------------
.params_edgespace <- function(params, mapping, edges){
  
  if(is.null(params[["colour"]]) && is.null(mapping[["colour"]])){
    if("edgeLineColor" %in% names(edges) ){
      params[["colour"]] <- edges[["edgeLineColor"]]
    }
  }
  
  if(is.null(params[["linewidth"]]) && is.null(mapping[["linewidth"]])){
    if("edgeLineWidth" %in% names(edges) ){
      params[["linewidth"]] <- edges[["edgeLineWidth"]]
    }
  }
  
  if(is.null(params[["linetype"]]) && is.null(mapping[["linetype"]])){
    if("edgeLineType" %in% names(edges) ){
      params[["linetype"]] <- edges[["edgeLineType"]]
    }
  }
  
  if(is.null(params[["alpha"]]) && is.null(mapping[["alpha"]])){
    if("edgeAlpha" %in% names(edges) ){
      params[["alpha"]] <- edges[["edgeAlpha"]]
    }
  }
  
  params
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
    linejoin = "mitre", na.rm = FALSE, .size_unit = "mm", 
    .nodes = NULL) {
    
    if(is.null(.nodes)){
      data <- .geom_adj_edge_offsets(data, size_unit = .size_unit)
    } else {
      data <- .geom_remap_edge_offsets(data, .nodes, size_unit = .size_unit)
    }
    
    data$arrow_size <- (arrow_size %||% 1)
    data$arrow_offset <- arrow_offset %||% 0
    
    coords <- coord$transform(data, panel_params)
    
    coords <- .geom_adj_arrow_offsets(coords)
    
    coords <- .geom_adj_arrow_size(coords, size_unit = .size_unit)
    
    # Create edge grobs
    grobs <- .get_edge_grobs(coords, lineend = lineend, 
      linejoin = linejoin, size_unit = .size_unit)
    
    grid::gTree(children = grobs,
      name = grid::grobName(prefix = "geom_edgespace")
    )
    
  },
  draw_key = draw_key_path
)

#-------------------------------------------------------------------------------
# --- Calculate Edge Offset ---
# The offset is the effective node radius: (size/2) + (stroke/2).
# 1. 'size' is the node diameter in points (mm * .pt). We use half for the radius.
# 2. 'stroke' is approx. 0.75 mm per unit (see 'aes_linetype_size_shape');
#    it's pre-processed by gg_par() as (stroke * .stroke / 2).
# 3. Since the border sits half-in/half-out of the node's edge, we rectify 
#    'stroke' to find the actual external thickness added to the radius.
# 4. Final sum is converted to 'npc' for grid coordinate alignment.
.geom_remap_edge_offsets <- function(edges, nodes, size_unit){
  mm2npc <- grid::convertWidth(unit(1, "mm"), unitTo = "npc", valueOnly = T)
  if(size_unit=="mm"){
    # input 'size' in 'mm', scaled to 'npc' by mm2npc
    n_offsets <- nodes[["size"]]/2 * mm2npc
  } else {
    # input 'size' in [0, 100], transformed to 'npc' by 0.01
    n_offsets <- nodes[["size"]]/2 * 0.01
  }
  # input 'stroke' and 'linewidth' in 'mm', scaled to 'npc'
  n_offsets <- n_offsets + ( nodes[["stroke"]]/2 * 0.75 * mm2npc )
  e_offsets <-  edges[["linewidth"]] * 0.75 * mm2npc
  
  emode <- .get_emode(edges[["arrowType"]])
  edges$offset_start <- ifelse(emode %in% c(0,1), 0, 
    n_offsets[edges[["vertex1"]]] + e_offsets)
  edges$offset_end <- ifelse(emode %in% c(0,2), 0, 
    n_offsets[edges[["vertex2"]]] + e_offsets)
  
  return(edges)
  
}

#-------------------------------------------------------------------------------
# Adjust offsets to 'size_unit', add 'linewidth' and a default 'stroke'
.geom_adj_edge_offsets <- function(edges, size_unit){
  
  if(size_unit=="mm"){
    sz2npc <- grid::convertWidth(unit(1, "mm"), unitTo = "npc", valueOnly = T)
  } else {
    sz2npc <- 0.01
  }
  stroke <- 0.5 * 0.75 * sz2npc
  linewidth <-  edges[["linewidth"]] * 0.75 * sz2npc
  offset_start <- (edges[["offset_start"]]/2 * sz2npc) + linewidth + stroke
  offset_end <- (edges[["offset_end"]]/2 * sz2npc) + linewidth + stroke
  
  emode <- .get_emode(edges[["arrowType"]])
  edges$offset_start <- ifelse(emode %in% c(0,1), 0, offset_start)
  edges$offset_end <- ifelse(emode %in% c(0,2), 0, offset_end)
  
  return(edges)
  
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
    edges$arrow_size <- edges[["arrow_size"]] * ggplot2::.pt
  } else {
    edges$arrow_size <- edges[["arrow_size"]] * ggplot2::.pt * 0.01
  }
  return(edges)
}

#-------------------------------------------------------------------------------
# segmentsGrob
.get_edge_grobs <- function(edges, lineend = "butt", 
  linejoin = "mitre", size_unit = "npc"){
  
  if(.empty(edges)){
    return( zeroGrob() )
  }
  
  edges$colour <- scales::alpha(edges$colour, edges$alpha)
  
  edges <- .set_arrows(edges)
  
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
### Adjust arrows
################################################################################
.set_arrows <- function(edges){
  edges <- .add_arrow_angle(edges)
  edges <- .adjust_arrow_size(edges)
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
.adjust_arrow_size <- function(edges){
  edges$arrowSize1 <- edges[["arrow_size"]]
  edges$arrowSize2 <- edges[["arrow_size"]]
  a_theta <- 60 #default angle to arrow sides
  a_theta <- a_theta / 180 * pi
  idx <- edges$arrowAngleStart==90
  if(any(idx, na.rm = TRUE)){
    l <- edges$arrowSize1[idx]/2
    b <- sqrt( (l^2 + l^2) - (2 * l^2) * cos(a_theta))
    edges$arrowSize1[idx] <- b + edges$linewidth[idx]/4
  }
  idx <- edges$arrowAngleEnd==90
  if(any(idx, na.rm = TRUE)){
    l <- edges$arrowSize2[idx]/2
    b <- sqrt( (l^2 + l^2) - (2 * l^2) * cos(a_theta))
    edges$arrowSize2[idx] <- b + edges$linewidth[idx]/4
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
### Construct arrows
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
  exy$xend <- exy$x + (edges$px * 0.01)
  exy$yend <- exy$y + (edges$py * 0.01)
  arrow <- grid::arrow(angle = edges$arrowAngleStart,
    type = "open", ends = "first",
    length = grid::unit(edges$arrowSize1, size_unit))
  return(list(exy=exy, arrow = arrow, color = edges$colour, 
    linewidth = edges$linewidth))
}
.arrow_ends <- function(edges, size_unit){
  exy <- edges[,c("x", "y", "xend", "yend")]
  exy$x <- exy$xend - (edges$px * 0.01)
  exy$y <- exy$yend - (edges$py * 0.01)
  arrow <- grid::arrow(angle = edges$arrowAngleEnd,
    type = "open", ends = "last",
    length = grid::unit(edges$arrowSize2, size_unit))
  return(list(exy=exy, arrow = arrow, color = edges$colour, 
    linewidth = edges$linewidth))
}


