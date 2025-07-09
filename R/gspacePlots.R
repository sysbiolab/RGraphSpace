
################################################################################
### ggplot2 protypes for GraphSpace-class methods
################################################################################
GeomNodeSpace <- ggproto("GeomNodeSpace", Geom,
    required_aes = c("x", "y", "size"),
    default_aes = aes(shape = 21, size = grid::unit(0.1, "npc"), 
        fill = "grey90", colour = "grey20", stroke = 1, alpha = NA),
    draw_panel = function(data, panel_scales, coord) {
        coords <- coord$transform(data, panel_scales)
        grid::pointsGrob(
            x = coords$x,
            y = coords$y,
            pch = coords$shape,
            # for pch in 0:25, size is about 75% of the
            # character height (see 'points()' graphics)
            size = coords$size * 1.25,
            default.units = "npc",
            gp = grid::gpar(
                fill = scales::alpha(coords$fill, coords$alpha), 
                col = scales::alpha(coords$colour, coords$alpha),
                lwd = coords$stroke
            )
        )
    }
)
.geom_nodespace <- function(mapping = NULL, data = NULL, 
    stat = "identity", position = "identity", na.rm = TRUE, 
    show.legend = FALSE, inherit.aes = TRUE, ...) {
    ggplot2::layer(
        geom = GeomNodeSpace, mapping = mapping,  
        data = data, stat = stat, position = position, 
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

################################################################################
### ggplot2 calls for GraphSpace-class methods
################################################################################
.set.gspace <- function(nodes, xlab, ylab, cl){
    x <- y <- NULL
    ggp <- ggplot2::ggplot(nodes, ggplot2::aes(x, y)) +
        ggplot2::scale_x_continuous(name = xlab, breaks = cl$axis.ticks,
            labels = format(cl$axis.ticks), position = cl$x.position,
            limits = cl$xylim, expand = ggplot2::expansion(mult = 0)) +
        ggplot2::scale_y_continuous(name = ylab, breaks = cl$axis.ticks,
            labels = format(cl$axis.ticks), limits = cl$xylim,
            expand = ggplot2::expansion(mult = 0)) +
        ggplot2::coord_fixed()
    return(ggp)
}

#-------------------------------------------------------------------------------
.add.labels1 <- function(ggp, nodes, node.labels, label.size, label.color){
  node.labels <- node.labels[!duplicated(node.labels)]
  if (is.null(names(node.labels))) names(node.labels) <- node.labels
  names(node.labels) <- ifelse(is.na(names(node.labels)), node.labels, names(node.labels))
  names(node.labels) <- ifelse(names(node.labels) == "", node.labels, names(node.labels))
  idx_df <- .get.label.idx(node.labels, nodes)
  if(any(is.na(idx_df$idx))){
    stop("All 'node.labels' should be annotated in the 'PathwaySpace' object.",
      call. = FALSE)
  }
  nodes_ft <- nodes[idx_df$idx, , drop = FALSE]
  nodes_ft$ID <- idx_df$label
  ID <- NULL
  ggp <- ggp + ggplot2::geom_text(mapping = aes(label = ID), 
    data = nodes_ft, fontface = "bold", size.unit = "mm",
    size = label.size, colour = label.color, 
    vjust="center", hjust="center")
  return(ggp)
}
.get.label.idx <- function(node.labels, nodes){
  idx_df <- data.frame(name=node.labels, label=names(node.labels))
  idx_df$name1 <- match(node.labels, nodes$name)
  idx_df$name2 <- match(names(node.labels), nodes$name)
  idx_df$labl1  <- match(node.labels, nodes$nodeLabel)
  idx_df$labl2  <- match(names(node.labels), nodes$nodeLabel)
  idx_df$idx <- sapply(seq_len(nrow(idx_df)), function(i){
    idx <- idx_df[i,seq.int(3,6)]
    idx[!is.na(idx)][1]
  })
  idx_df <- idx_df[,c(1,2,7)]
  return(idx_df)
}


#-------------------------------------------------------------------------------
.add.labels2 <- function(ggp, nodes){
  nodes_ft <- nodes[!is.na(nodes$nodeLabel), ]
  nodeLabel <- NULL
  ggp <- ggp + geom_text(mapping = aes(label = nodeLabel), 
    data = nodes_ft, fontface = "bold",
    size = nodes_ft$nodeLabelSize, 
    colour = nodes_ft$nodeLabelColor, 
    size.unit = "pt",
    vjust="center", hjust="center")
  return(ggp)
}

#-------------------------------------------------------------------------------
.add.image <- function(ggp, image){
  ggp <- ggp + annotation_raster(image, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  return(ggp)
}

#-------------------------------------------------------------------------------
.add.graph <- function(ggp, nodes, edges){
    if(nrow(edges)>0){
      edges <- .offset.edges(nodes, edges)
      ggp <- .add.segments(ggp, edges)
      ggp <- .add.arrows(ggp, edges) 
    }
    ggp <- .add.nodes(ggp, nodes)
    return(ggp)
}
.add.nodes <- function(ggp, nodes){
    fill_color <- main_color <- nodes$nodeColor
    idx <- nodes$nodeShape >= 21
    main_color[idx] <-  nodes$nodeLineColor[idx]
    ggp <- ggp + .geom_nodespace(size = grid::unit(nodes$nodeSize, "npc"), 
        fill = fill_color, colour = main_color, 
        shape=nodes$nodeShape, stroke = nodes$nodeLineWidth)
    return(ggp)
}
.add.segments <- function(ggp, edges){
    linetype <- levels(as.factor(edges$edgeLineType))
    names(linetype) <- linetype
    x1 <- y1 <- x2 <- y2 <- edgeLineType <- NULL
    ggp <- ggp + 
      ggplot2::geom_segment(
        aes(x = x1, y = y1, xend = x2, yend = y2, linetype=edgeLineType), 
        data = edges, linewidth = edges$edgeLineWidth, linejoin = "mitre", 
        colour = edges$edgeLineColor, show.legend = TRUE) +
      scale_linetype_manual(values=linetype)
    return(ggp)
}
.add.arrows <- function(ggp, edges){
    x1 <- y1 <- x2 <- y2 <- NULL
    #--- add arrow ends
    idx <- edges$emode==1 | edges$emode==3
    if(sum(idx)>0){
        edges2 <- edges[idx,]
        ends <-.get.arrow.ends(edges2)
        ggp <- ggp + ggplot2::geom_segment(
            aes(x = x1, y = y1, xend = x2, yend = y2), 
            data = ends$exy, linewidth = edges2$edgeLineWidth, 
            linetype = 1, linejoin = "mitre", colour = edges2$edgeLineColor, 
            arrow = ends$arrow, show.legend = FALSE)
    }
    #--- add arrow starts
    idx <- edges$emode==2 | edges$emode==3
    if(sum(idx)>0){
        edges2 <- edges[idx,]
        starts <-.get.arrow.starts(edges2)
        ggp <- ggp + ggplot2::geom_segment(
            aes(x = x1, y = y1, xend = x2, yend = y2), 
            data = starts$exy, linewidth = edges2$edgeLineWidth, 
            linetype = 1, linejoin = "mitre", colour = edges2$edgeLineColor, 
            arrow = starts$arrow, show.legend = FALSE)
    }
    return(ggp)
}

#-------------------------------------------------------------------------------
.offset.edges <- function(nodes, edges){
    offset <- nodes$nodeSize/2 + 0.02
    edges$offset.start <- ifelse(edges$emode %in% c(0,1), 0, 
        offset[edges$vertex1])
    edges$offset.end <- ifelse(edges$emode %in% c(0,2), 0, 
        offset[edges$vertex2])
    edges <- .offset.exy(edges)
    return(edges)
}
.offset.exy <- function(edges){
    dx <- edges$x2 - edges$x1
    dy <- edges$y2 - edges$y1
    dist <- sqrt( dx^2 + dy^2 )
    edges$px <- dx/dist
    edges$py <- dy/dist
    edges$x1 <- edges$x1 + (edges$px * edges$offset.start)
    edges$y1 <- edges$y1 + (edges$py * edges$offset.start)
    edges$x2 <- edges$x2 - (edges$px * edges$offset.end)
    edges$y2 <- edges$y2 - (edges$py * edges$offset.end)
    return(edges)
}

#-------------------------------------------------------------------------------
.get.arrow.ends <- function(edges){
    exy <- edges[,c("x1", "y1", "x2", "y2")]
    exy$x1 <- exy$x2 - (edges$px * 0.01)
    exy$y1 <- exy$y2 - (edges$py * 0.01)
    arrow <- grid::arrow(angle = edges$arrowAngle_2,
        type = "open", ends = "last",
        length = grid::unit(edges$arrowLength_2 * ggplot2::.pt, "mm"))
    return(list(exy=exy, arrow=arrow))
}
.get.arrow.starts <- function(edges){
    exy <- edges[,c("x1", "y1", "x2", "y2")]
    exy$x2 <- exy$x1 + (edges$px * 0.01)
    exy$y2 <- exy$y1 + (edges$py * 0.01)
    arrow <- grid::arrow(angle = edges$arrowAngle_1,
        type = "open", ends = "first",
        length = grid::unit(edges$arrowLength_1 * ggplot2::.pt, "mm"))
    return(list(exy=exy, arrow=arrow))
}

#-------------------------------------------------------------------------------
.custom.themes <- function(gg, theme, font.size, bg.color) {
    et1 <- ggplot2::element_text(size = 14 * font.size)
    et2 <- ggplot2::element_text(size = 12 * font.size)
    if (theme == "th3") {
        gg <- .custom.th3(gg, font.size, bg.color)
    } else if (theme == "th2") {
        gg <- .custom.th2(gg, font.size, bg.color)
    } else if (theme == "th1") {
      gg <- .custom.th1(gg, font.size, bg.color) 
    } else {
        gg <- .custom.th0(gg, font.size, bg.color)
    }
    return(gg)
}
.custom.th0 <- function(gg, font.size, bg.color) {
    et1 <- ggplot2::element_text(size = 14 * font.size)
    et2 <- ggplot2::element_text(size = 12 * font.size)
    gg <- gg + ggplot2::theme(axis.title = et1, axis.text = et2,
        legend.title = et2, legend.text = et2, legend.position = "none",
        panel.background = element_rect(fill = bg.color))
    return(gg)
}
.custom.th1 <- function(gg, font.size, bg.color) {
  et1 <- ggplot2::element_text(size = 14 * font.size)
  et2 <- ggplot2::element_text(size = 12 * font.size)
  gg <- gg + ggplot2::theme_bw() +
    ggplot2::theme(axis.title = et1,
      axis.text = et2, legend.title = et2,
      legend.text = et2, legend.margin = margin(0, 0, 0, 0), 
      plot.margin = margin(1, 1, 1, 1), 
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      panel.grid.minor = element_line(linewidth = 0.7, 
        colour = bg.color),
      panel.grid.major = element_line(linewidth = 0.7,
        colour = bg.color),
      legend.position = "none",
      axis.ticks = element_line(linewidth = 0.7),
      axis.line = element_blank(),
      panel.border = element_rect(linewidth = 1.2))
  return(gg)
}
.custom.th2 <- function(gg, font.size, bg.color) {
    et1 <- ggplot2::element_text(size = 14 * font.size)
    et2 <- ggplot2::element_text(size = 12 * font.size)
    gg <- gg + ggplot2::theme_gray() + ggplot2::theme(axis.title = et1,
        axis.text = et2, legend.title = et2,
        legend.text = et2, legend.margin = margin(0, 0, 0, 0), 
        plot.margin = margin(5, 10, 0, 10), 
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.ticks = element_line(linewidth = 0.7),
        axis.line = element_blank(), panel.border = element_blank(),
        panel.background = element_rect(fill = bg.color))
    return(gg)
}
.custom.th3 <- function(gg, font.size, bg.color) {
    et1 <- ggplot2::element_text(size = 14 * font.size)
    et2 <- ggplot2::element_text(size = 12 * font.size)
    gg <- gg + ggplot2::theme_gray() + 
        ggplot2::theme(axis.title = et1, axis.text = et2, 
        legend.title = element_text(size = 12 * font.size, vjust = 1), 
        legend.text = et2,
        legend.margin = margin(0, 0, 0, 0),
        legend.position = "none",
        plot.margin = margin(5, 5, 5, 5), 
        legend.box.margin = margin(0, 0, 0, 0), 
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_line(linewidth = 0.5),
        axis.line = element_blank(), panel.border = element_blank(),
        panel.background = element_rect(fill = bg.color))
    return(gg)
}
.set.theme.bks <- function(theme, cl=list()){
    if (theme %in% c("th3")) {
        cl$axis.ticks <- c(0.25, 0.5, 0.75)
        cl$xylim <- c(-0.01, 1.01)
        cl$x.position <- "top"
        cl$justify <- "centre"
    } else if (theme %in% c("th2")) {
        cl$axis.ticks <- seq(0.1, 0.9, 0.2)
        cl$xylim <- c(-0.01, 1.01)
        cl$x.position <- "bottom"
        cl$justify <- "right"
    } else {
        cl$axis.ticks <- seq(0, 1, 0.2)
        cl$xylim <- c(-0.05, 1.05)
        cl$x.position <- "bottom"
        cl$justify <- "right"
    }
    return(cl)
}
