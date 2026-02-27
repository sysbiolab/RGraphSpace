
#-------------------------------------------------------------------------------
#' @title GeomNodeSpace: a ggplot2 prototype for GraphSpace-class methods
#'
#' @description
#' 
#' `GeomNodeSpace` is the underlying ggproto object used by 
#' \code{\link{geom_nodespace}} to draw node elements in a 2D graph layout. 
#' It produces point-like glyphs (typically circles) whose properties are 
#' controlled by aesthetics such as `x`, `y`, `size` and `colour`.
#'
#' This geom is designed for graph/network diagrams, where node
#' attributes are often already in their final form (e.g., hex colors).
#' When used with identity scales, the appearance of nodes can be
#' fully controlled by the input data.
#'
#' @section Aesthetics:
#'
#' `GeomNodeSpace` understands the following aesthetics:
#'
#' \itemize{
#'   \item `x` Horizontal position in `npc` \code{\link[grid]{unit}} (required).
#'   \item `y` Vertical position in `npc` \code{\link[grid]{unit}} (required).
#'   \item `size` Node size in `npc` \code{\link[grid]{unit}} (required).
#'   \item `fill` Node fill colour.
#'   \item `colour` Node line colour.
#'   \item `shape` Integer code between 0 and 25 
#'   (see \code{\link[graphics]{points}}).
#'   \item `linewidth` Line width, using 'lwd' standard graphics unit 
#'   (see \code{\link[grid]{gpar}}).
#'   \item `alpha` Transparency applied to fill and line colour.
#' }
#'
#' These aesthetics follow ggplot2's conventions for point-like `geoms`.
#'
#' @section Drawing:
#'
#' The `draw_panel()` method transforms data into panel coordinates and
#' constructs a `grid::pointsGrob()` for rendering. Node size is expected 
#' to be normalized to the range `[0, 1]`, so it can be interpreted in graph 
#' space using `npc` unit.
#'
#' @seealso
#' \code{\link{geom_nodespace}}, \code{\link[ggplot2]{geom_point}}
#'
#' @importFrom ggplot2 scale_colour_identity draw_key_point
#' @importFrom ggplot2 geom_point geom_segment aes Geom .pt geom_text gg_par
#' @importFrom ggplot2 element_rect margin element_blank layer theme_bw
#' @importFrom ggplot2 element_line element_text ggproto theme theme_gray
#' @importFrom ggplot2 scale_linetype_manual annotation_raster coord_fixed
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous expansion labs
#' @importFrom ggplot2 expansion
#' @importFrom grid gpar arrow unit pointsGrob segmentsGrob grobTree gList
#' @importFrom scales alpha
#' @export
GeomNodeSpace <- ggproto(
  
  "GeomNodeSpace", ggplot2::Geom, 
  
  required_aes = c("x", "y"),
  
  default_aes = aes(fill = "grey90", colour = "grey20",
    shape = 21, size = 0.05, linewidth = 1, alpha = 1),
  
  draw_panel = function(data, panel_params, coord) {
    
    coord <- coord$transform(data, panel_params)
    
    grid::pointsGrob(
      x = coord$x,
      y = coord$y,
      pch = coord$shape,
      size = grid::unit(coord$size, "npc"),
      default.units = "npc",
      gp = grid::gpar(
        fill = scales::alpha(coord$fill, coord$alpha), 
        col = scales::alpha(coord$colour, coord$alpha),
        lwd = coord$linewidth
      )
    )
  },
  
  draw_key = ggplot2::draw_key_point
  
)

#-------------------------------------------------------------------------------
#' @title Draw node elements in a 2D graph layout
#' 
#' @description
#' Constructor for \code{\link{GeomNodeSpace}} ggproto objects, a variant 
#' of \code{\link[ggplot2]{geom_point}} supporting node attributes from 
#' \linkS4class{GraphSpace} objects.
#'
#' This geom is designed to create node-level aesthetics such as
#' `size`, `fill`, `colour`, or any custom aesthetics 
#' defined in `GeomNodeSpace`.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' These mappings apply to the layer and may override global aesthetics.
#'
#' @param data Either a \linkS4class{GraphSpace} object or a data frame 
#' containing node attributes. If `NULL`, the layer will use the default 
#' plot data.
#'
#' @param stat The statistical transformation to use on the data.
#' Defaults to `"identity"`.
#'
#' @param position Position adjustment, either as a string or
#' the result of a call to a position adjustment function.
#'
#' @param ... Additional parameters passed to the underlying
#' drawing function in `GeomNodeSpace`.
#'
#' @param na.rm Logical. Should missing values be removed?
#' Defaults to `TRUE`.
#'
#' @param show.legend Logical or a named logical vector indicating
#' whether this layer should be included in legends.
#'
#' @param inherit.aes Logical. If `FALSE` (default), the layer will use 
#' aesthetics defined in `mapping`. Set to `TRUE` if this geom should 
#' inherit aesthetics from the plot.
#'
#' @return A ggplot2 layer that renders node glyphs defined by
#' `GeomNodeSpace`.
#'
#' @section Aesthetics:
#'
#' `geom_nodespace` understands the following aesthetics:
#'
#' \itemize{
#'   \item `x` Horizontal position in `npc` \code{\link[grid]{unit}} (required).
#'   \item `y` Vertical position in `npc` \code{\link[grid]{unit}} (required).
#'   \item `size` Node size in `npc` \code{\link[grid]{unit}} (required).
#'   \item `fill` Node fill colour.
#'   \item `colour` Node line colour.
#'   \item `shape` Integer code between 0 and 25 
#'   (see \code{\link[graphics]{points}}).
#'   \item `linewidth` Line width, using 'lwd' standard graphics unit 
#'   (see \code{\link[grid]{gpar}}).
#'   \item `alpha` Transparency applied to fill and line colour.
#' }
#' 
#' @seealso
#' \code{\link{GeomNodeSpace}}, \linkS4class{GraphSpace}
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
#' ggplot() + geom_nodespace(aes(x = x, y = y), data = gs)
#' 
#' }
#' 
#' @importFrom rlang list2
#' @export
geom_nodespace <- function(mapping = NULL, data = NULL, 
  stat = "identity", position = "identity", ..., na.rm = TRUE, 
  show.legend = NA, inherit.aes = FALSE) {
  if (inherits(data, "GraphSpace")) {
    data <- gs_nodes(data)
  }
  ggplot2::layer(
    geom = GeomNodeSpace, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list2(na.rm = na.rm, ...)
  )
}

#-------------------------------------------------------------------------------
#' @title GeomEdgeSpace: a ggplot2 prototype for GraphSpace-class methods
#'
#' @description
#' 
#' `GeomEdgeSpace` is the underlying ggproto object used by 
#' \code{\link{geom_edgespace}} to draw edge elements in a 2D graph layout, 
#' whose properties are controlled by aesthetics such as `colour` 
#' and `linewidth`.
#'
#' This `geom` is designed for graph/network diagrams, where edge
#' attributes are often already in their final form (e.g., hex colors).
#' When used with identity scales, the appearance of edges can be
#' fully controlled by the input data.
#' 
#' @section Aesthetics:
#'
#' `GeomEdgeSpace` understands the following aesthetics:
#'
#' \itemize{
#'   \item `x` Starting horizontal position in 
#'   `npc` \code{\link[grid]{unit}} (required).
#'   \item `y` Starting vertical position in 
#'   `npc` \code{\link[grid]{unit}} (required).
#'   \item `xend` Ending horizontal position in 
#'   `npc` \code{\link[grid]{unit}} (required).
#'   \item `yend` Ending vertical position in 
#'   `npc` \code{\link[grid]{unit}} (required).
#'   \item `colour` Edge colour.
#'   \item `linewidth` Edge width, using 'lwd' standard graphics unit 
#'   (see \code{\link[grid]{gpar}}).
#'   \item `linetype` Line type, using 'lty' standard graphics unit 
#'   (see \code{\link[grid]{gpar}}).
#'   \item `alpha` Transparency applied to the edge colour.
#' }
#'
#' These aesthetics follow ggplot2's conventions for segment-like `geoms`.
#' 
#' @seealso
#' \code{\link{geom_edgespace}}, \code{\link[ggplot2]{geom_segment}}
#'
#' @importFrom ggplot2 remove_missing
#' @export
GeomEdgeSpace <- ggproto(
  
  "GeomEdgeSpace", ggplot2::Geom,
  
  required_aes = c("x", "y", "xend", "yend"),
  
  default_aes = ggplot2::aes(
    colour = "grey80",
    linewidth = 0.5,
    linetype = "solid",
    alpha = 1
  ),
  
  draw_panel = function(data, panel_params, coord, 
    lineend = "butt", linejoin = "mitre", arrow_size = 1, 
    arrow_type = 0, offset_start = 0, offset_end = 0) {

    data$xend <- data$xend %||% data$x
    data$yend <- data$yend %||% data$y
    
    data$arrow_size <- arrow_size 
    data$arrow_type <- arrow_type
    data$offset_start <- offset_start
    data$offset_end <- offset_end
    data <- .set_arrows(data)
    
    coord <- coord$transform(data, panel_params)
    
    arrows <- .get_arrows(coord)
    
    grobs <- list()
    if (nrow(coord)>0) {
      grobs[[length(grobs) + 1]] <- grid::segmentsGrob(
        x0 = coord$x,
        y0 = coord$y,
        x1 = coord$xend,
        y1 = coord$yend,
        default.units = "npc",
        gp = ggplot2::gg_par(
          col  = scales::alpha(coord$colour, alpha = coord$alpha),
          lwd  = coord$linewidth, lty  = coord$linetype,
          lineend = lineend, linejoin = linejoin
        )
      )
    }

    if (!is.null(arrows$ends)) {
      grobs[[length(grobs) + 1]] <- grid::segmentsGrob(
        x0 = arrows$ends$exy$x,
        y0 = arrows$ends$exy$y,
        x1 = arrows$ends$exy$xend,
        y1 = arrows$ends$exy$yend,
        arrow = arrows$ends$arrow,
        gp = ggplot2::gg_par(
          col  = scales::alpha(arrows$ends$color, alpha = 1),
          lwd  = arrows$ends$linewidth, lty  = "solid",
          lineend = lineend, linejoin = linejoin
        )
      )
    }

    if (!is.null(arrows$starts)) {
      grobs[[length(grobs) + 1]] <- grid::segmentsGrob(
        x0 = arrows$starts$exy$x,
        y0 = arrows$starts$exy$y,
        x1 = arrows$starts$exy$xend,
        y1 = arrows$starts$exy$yend,
        arrow = arrows$starts$arrow,
        gp = ggplot2::gg_par(
          col  = scales::alpha(arrows$starts$color, alpha = 1),
          lwd  = arrows$starts$linewidth, lty  = "solid",
          lineend = lineend, linejoin = linejoin
        )
      )
    }
    
    grid::grobTree(children = do.call(grid::gList, grobs))
    
  },
  draw_key = draw_key_path
)

#-------------------------------------------------------------------------------
#' @title Draw edge elements in a 2D graph layout
#' 
#' @description
#' 
#' Constructor for \code{\link{GeomEdgeSpace}} ggproto objects, a variant 
#' of \code{\link[ggplot2]{geom_segment}} supporting edge attributes and 
#' bidirectional arrows.
#'
#' This `geom` is designed to create edge-level aesthetics such as `colour` 
#' and `linewidth`, or any custom aesthetics defined in `GeomEdgeSpace`.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' These mappings apply to the layer and may override global aesthetics.
#'
#' @param data Either a \linkS4class{GraphSpace} object or a data frame 
#' containing edge attributes. If `NULL`, the layer will use the default 
#' plot data.
#'
#' @param stat The statistical transformation to use on the data.
#' Defaults to `"identity"`.
#'
#' @param position Position adjustment, either as a string or
#' the result of a call to a position adjustment function.
#'
#' @param ... Additional parameters passed to the underlying
#' drawing function in `GeomEdgeSpace`.
#'
#' @param show.legend Logical or a named logical vector indicating
#' whether this layer should be included in legends.
#'
#' @param inherit.aes Logical. If `FALSE` (default), the layer will use 
#' aesthetics defined in `mapping`. Set to `TRUE` if this geom should 
#' inherit aesthetics from the plot.
#'
#' @param lineend Line end style (round, butt, square).
#' 
#' @param linejoin Line join style (round, mitre, bevel).
#' 
#' @param arrow_type Arrowhead types (see 'drawing' section).
#' 
#' @param arrow_size Numeric scaling factor controlling arrowhead geometry.
#' 
#' @param offset_start Offset arrows as a fraction of graph space at
#' start positions.
#' 
#' @param offset_end Offset arrows as a fraction of graph space at
#' end positions.
#'   
#' @return A ggplot2 layer that renders edge segments defined by
#' \code{\link{GeomEdgeSpace}}.
#' 
#' @section Aesthetics:
#' 
#' `geom_edgespace` understands the following aesthetics:
#'
#' \itemize{
#'   \item `x` Starting horizontal position in 
#'   `npc` \code{\link[grid]{unit}} (required).
#'   \item `y` Starting vertical position in 
#'   `npc` \code{\link[grid]{unit}} (required).
#'   \item `xend` Ending horizontal position in 
#'   `npc` \code{\link[grid]{unit}} (required).
#'   \item `yend` Ending vertical position in 
#'   `npc` \code{\link[grid]{unit}} (required).
#'   \item `colour` Edge colour.
#'   \item `linewidth` Edge width, using 'lwd' standard graphics unit 
#'   (see \code{\link[grid]{gpar}}).
#'   \item `linetype` Line type, using 'lty' standard graphics unit 
#'   (see \code{\link[grid]{gpar}}).
#'   \item `alpha` Transparency applied to the edge colour.
#' }
#' 
#' @details
#' 
#' Arrowhead size is a numeric scaling factor controlling arrowhead geometry. 
#' The value is interpreted in the same numeric space as line width (`lwd`), 
#' ensuring consistent scaling between edge strokes and arrowheads.
#' 
#' Arrowhead types can be specified using either integer or character codes:\cr
#' - No arrow: \code{0 or "---"}\cr
#' - Forward arrow: \code{1 or "-->"}\cr
#' - Backward arrow: \code{2 or "<--"}\cr
#' - Bidirectional arrow: \code{3 or "<->"}\cr
#' - Forward arrow with backward bar: \code{4 or "|->"}\cr
#' - Forward bar: \code{-1 or "--|"}\cr
#' - Backward bar: \code{-2 or "|--"}\cr
#' - Bidirectional bar: \code{-3 or "|-|"}\cr
#' - Backward arrow with forward bar: \code{-4 or "<-|"}\cr
#' 
#' @seealso
#' \code{\link{GeomEdgeSpace}}, \linkS4class{GraphSpace}
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
#' ggplot() + geom_edgespace(aes(x = x, y = y, xend = xend, yend = yend),
#'   data = gs) + theme(aspect.ratio = 1) + 
#'   scale_x_continuous(limits = c(0, 1)) +
#'   scale_y_continuous(limits = c(0, 1))
#' 
#' }
#' 
#' @export
geom_edgespace <- function(mapping = NULL, data = NULL,
  stat = "identity", position = "identity", ..., 
  show.legend = NA, inherit.aes = FALSE,
  lineend = "butt", linejoin = "mitre", arrow_type = 0, 
  arrow_size = 1,  offset_start = 0, offset_end = 0) {
  
  if (inherits(data, "GraphSpace")) {
    data <- gs_edges(data)
    params <- list2(
      arrow_type = data$arrowType, 
      arrow_size = arrow_size,
      offset_start = data$offsetStart, 
      offset_end = data$offsetEnd, ...)
  } else {
    arrow_type <- .transform_arrowtype(arrow_type)
    params <- list2(
      arrow_type = arrow_type, 
      arrow_size = arrow_size,
      offset_start = offset_start, 
      offset_end = offset_end, ...)
  }
  ggplot2::layer(
    geom = GeomEdgeSpace,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

################################################################################
### Adjust arrows for GeomNodeSpace
################################################################################
.set_arrows <- function(edges){
  if(is.null(edges$linewidth)) edges$linewidth <- 1
  if(is.null(edges$arrow_size)) edges$arrow_size <- 1
  if(is.null(edges$arrow_type)) edges$arrow_type <- 0
  if(is.null(edges$offset_start)) edges$offset_start <- 0
  if(is.null(edges$offset_end)) edges$offset_end <- 0
  
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
  edges$arrowAngleStart <- .a_start(edges$arrow_type)
  edges$arrowAngleEnd <- .a_end(edges$arrow_type)
  return(edges)
}
.adjust_arrow_size <- function(edges){
  edges$arrowSize1 <- edges$arrow_size
  edges$arrowSize2 <- edges$arrow_size
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
  emode <- .get_emode(edges$arrow_type)
  offset_start <- offset_end <- rep(0, nrow(edges))
  idx <- emode==2 | emode==3
  offset_start[idx] <- edges$offset_start[idx]
  idx <- emode==1 | emode==3
  offset_end[idx] <- edges$offset_end[idx]
  dx <- edges$xend - edges$x
  dy <- edges$yend - edges$y
  dist <- sqrt( dx^2 + dy^2 )
  edges$px <- dx/dist
  edges$py <- dy/dist
  edges$x <- edges$x + (edges$px * offset_start)
  edges$y <- edges$y + (edges$py * offset_start)
  edges$xend <- edges$xend - (edges$px * offset_end)
  edges$yend <- edges$yend - (edges$py * offset_end)
  return(edges)
}

################################################################################
### Construct arrows for GeomNodeSpace
################################################################################
.get_arrows <- function(edges){
  emode <- .get_emode(edges$arrow_type)
  #--- add arrow starts
  idx <- emode==2 | emode==3
  if(any(idx)){
    starts <- .arrow_starts(edges[idx,])
  } else {
    starts <- NULL
  }
  #--- add arrow ends
  idx <- emode==1 | emode==3
  if(any(idx)){
    ends <- .arrow_ends(edges[idx,])
  } else {
    ends <- NULL
  }
  arrows <- list(ends = ends, starts = starts)
  return(arrows)
}
.arrow_starts <- function(edges){
  exy <- edges[,c("x", "y", "xend", "yend")]
  exy$xend <- exy$x + (edges$px * 0.01)
  exy$yend <- exy$y + (edges$py * 0.01)
  arrow <- grid::arrow(angle = edges$arrowAngleStart,
    type = "open", ends = "first",
    length = grid::unit(edges$arrowSize1 * ggplot2::.pt, "mm"))
  return(list(exy=exy, arrow = arrow, color = edges$colour, 
    linewidth = edges$linewidth))
}
.arrow_ends <- function(edges){
  exy <- edges[,c("x", "y", "xend", "yend")]
  exy$x <- exy$xend - (edges$px * 0.01)
  exy$y <- exy$yend - (edges$py * 0.01)
  arrow <- grid::arrow(angle = edges$arrowAngleEnd,
    type = "open", ends = "last",
    length = grid::unit(edges$arrowSize2 * ggplot2::.pt, "mm"))
  return(list(exy=exy, arrow = arrow, color = edges$colour, 
    linewidth = edges$linewidth))
}


