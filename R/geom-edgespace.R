
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
#' @param data A \link{GraphSpace} object.
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
#' @param size_aes Optional node size mapping, created with [ggplot2::aes()]. 
#' Can be used to synchronize \link{geom_nodespace} and \link{geom_edgespace} 
#' to ensure directed edges and arrows are offset from node boundaries. 
#' This prevents overlaps when \link{geom_nodespace} includes a dynamic 
#' `size` or `stroke` mappings. For automated scale detection, use
#' \link{inject_nodespace} instead.
#' 
#' @param range Optional numeric vector of length 2 (default `c(1, 6)`) 
#' specifying the min/max node diameters in mm. Used with `size_aes` and 
#' should match the `range` in [ggplot2::scale_size_continuous()] for 
#' accurate edge clipping. For automated scale detection, use
#' \link{inject_nodespace} instead.
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
#' \code{\link{geom_nodespace}}, \link{GraphSpace}
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
geom_edgespace <- function(mapping = NULL, data = NULL,
  stat = "identity", position = "identity", ..., 
  na.rm = FALSE, show.legend = NA, inherit.aes = FALSE,
  size_aes = NULL, range = c(1, 6), 
  arrow_size = 1, arrow_offset = 0.01,
  lineend = "butt", linejoin = "mitre") {
  
  if (inherits(data, "GraphSpace")) {
    .geom_check_slots(data)
    nodes <- gs_nodes(data)
    edges <- gs_edges(data)
  } else {
    stop("'data' must be a 'GraphSpace' object.")
  }
  
  # Check custom params
  .validate_gs_args("singleLogical", "na.rm", na.rm)
  .validate_gs_args("singleNumber", "arrow_size", arrow_size)
  .validate_gs_args("singleNumber", "arrow_offset", arrow_offset)
  .validate_gs_args("singleString", "lineend", lineend)
  .validate_gs_args("singleString", "linejoin", linejoin)
  if (!is.null(size_aes) && !inherits(size_aes, "uneval")) {
    stop("Argument 'size_aes' must be a mapping created with aes().", 
      call. = FALSE)
  }
  if (!is.numeric(range) || length(range) != 2) {
    stop("Argument 'range' must be a numeric vector of length 2.", 
      call. = FALSE)
  }
  
  params <- list2(
    na.rm = na.rm, 
    arrow_size = arrow_size,
    arrow_offset = arrow_offset,
    lineend = lineend,
    linejoin = linejoin,
    size_aes = size_aes,
    range = range,
    .nodes = nodes,
    .size_unit = "mm",
    .size_rule = NULL,
    ...)
  
  mapping <- .mapping_edgespace(mapping)
  
  params <- .params_edgespace(params, mapping, edges)
  
  ggplot2::layer(
    geom = GeomEdgeSpace,
    stat = stat,
    mapping = mapping,
    data = edges,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
  
}

#-------------------------------------------------------------------------------
.mapping_edgespace <- function(mapping) {
  x <- y <- xend <- yend <- arrowType <- NULL
  vertex1 <- vertex2 <- NULL
  default_mapping <- ggplot2::aes(
    x = x, y = y, xend = xend, yend = yend,
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
  
  required_aes = c("x", "y", "xend", "yend", 
    "vertex1", "vertex2", "arrowType"),
  
  non_missing_aes = c("linewidth", "linetype", "colour"),
  
  setup_params = function(data, params){
    
    nodes <- .geom_map_size_aes(params$.nodes, size_aes = params$size_aes)
    
    if(is.null(params$.size_rule)){
      size_range <- params$range
    } else {
      size_range <- params$.size_rule$palette(c(0, 1))
    }
    
    params$.nodes <- .geom_check_node_size(nodes, 
      size_unit = params$.size_unit, range = size_range)
    
    params
  },
  
  default_aes = ggplot2::aes(
    linewidth = 0.5,
    linetype = "solid",
    colour = "grey80",
    alpha = NA,
    offset_start = 0,
    offset_end = 0
  ),
  
  draw_panel = function(self, data, panel_params, coord, .nodes,   
    arrow_size = 1, arrow_offset = 0.01, lineend = "butt", 
    linejoin = "mitre", na.rm = FALSE, size_aes = NULL, 
    range = c(1, 6), .size_unit = "mm", .size_rule = NULL) {
    
    data <- .geom_get_edge_offsets(data, .nodes, size_unit = .size_unit)
    
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
.geom_map_size_aes <- function(nodes, size_aes){
  
  required_att <- c("x", "y", "vertex", "size", "stroke")
  eval_att <- c(size = "nodeSize", stroke = "nodeLineWidth")
  default_att <- .get_default_vatt()
  
  if (!is.null(size_aes) && !inherits(size_aes, "uneval")) {
    size_aes <- NULL
    warning("Argument 'size_aes' must be a mapping created with aes().", 
      call. = FALSE)
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

#-------------------------------------------------------------------------------
.is_valid_aes <- function(att, mapping, data_df) {
  if (is.null(mapping)) return(FALSE)
  expr <- mapping[[att]]
  if (is.null(expr)) return(FALSE)
  vars <- all.vars(expr)
  if (length(vars) != 1) return(FALSE)
  vars %in% names(data_df)
}

#-------------------------------------------------------------------------------
.geom_get_edge_offsets <- function(edges, nodes, size_unit){
  
  # --- Calculate Edge Offset ---
  # The offset is the effective node radius: (size/2) + (stroke/2).
  # 1. 'size' is the node diameter in points (mm * .pt). We use half for the radius.
  # 2. 'stroke' is approx. 0.75 mm per unit (see 'aes_linetype_size_shape');
  #    it's pre-processed by gg_par() as (stroke * .stroke / 2).
  # 3. Since the border sits half-in/half-out of the node's edge, we rectify 
  #    'stroke' to find the actual external thickness added to the radius.
  # 4. Final sum is converted to 'npc' for grid coordinate alignment.
  mm2npc <- grid::convertWidth(unit(1, "mm"), unitTo = "npc", valueOnly = T)
  if(size_unit=="mm"){
    # input 'size' and 'stroke' in 'mm', scaled to 'npc' by mm2npc
    offsets <- (nodes[["size"]]/2 + nodes[["stroke"]]/2 * 0.75) * mm2npc
  } else {
    # input 'size' in [0, 100], transformed to 'npc' by 0.01
    offsets <- (nodes[["size"]]/2 * 0.01) + ( nodes[["stroke"]]/2 * 0.75) * mm2npc
  }
  emode <- .get_emode(edges[["arrowType"]])
  edges$offset_start <- ifelse(emode %in% c(0,1), 0, offsets[edges[["vertex1"]]])
  edges$offset_end <- ifelse(emode %in% c(0,2), 0, offsets[edges[["vertex2"]]])
  
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
  total_offset <- offset_start + offset_end
  
  dx <- edges$xend - edges$x
  dy <- edges$yend - edges$y
  L <- sqrt( dx^2 + dy^2 )
  L <- ifelse(L == 0, 1e-6, L)
  
  scale <- pmin(1, L / total_offset)
  offset_start <- offset_start * scale
  offset_end <- offset_end * scale
  
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


