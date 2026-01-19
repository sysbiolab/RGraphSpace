
################################################################################
### Plotting adjusts for GraphSpace-class methods
################################################################################
.set_gspace <- function(theme){
  bks <- .get_breaks(theme)
  ggp <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(breaks = bks$axis.ticks,
      labels = format(bks$axis.ticks), position = bks$x.position,
      limits = bks$xylim, expand = ggplot2::expansion(mult = 0)) +
    ggplot2::scale_y_continuous(breaks = bks$axis.ticks,
      labels = format(bks$axis.ticks), limits = bks$xylim,
      expand = ggplot2::expansion(mult = 0)) + ggplot2::coord_fixed()
  return(ggp)
}
.get_breaks <- function(theme){
  bks <- list()
  if (theme %in% c("th3")) {
    bks$axis.ticks <- c(0.25, 0.5, 0.75)
    bks$xylim <- c(-0.01, 1.01)
    bks$x.position <- "top"
    bks$justify <- "centre"
  } else if (theme %in% c("th2")) {
    bks$axis.ticks <- seq(0.1, 0.9, 0.2)
    bks$xylim <- c(-0.01, 1.01)
    bks$x.position <- "bottom"
    bks$justify <- "right"
  } else {
    bks$axis.ticks <- seq(0, 1, 0.2)
    bks$xylim <- c(-0.05, 1.05)
    bks$x.position <- "bottom"
    bks$justify <- "right"
  }
  return(bks)
}

#-------------------------------------------------------------------------------
.add_labels1 <- function(ggp, nodes, node.labels, label.size, label.color){
  node.labels <- node.labels[!duplicated(node.labels)]
  if (is.null(names(node.labels))) names(node.labels) <- node.labels
  names(node.labels) <- ifelse(is.na(names(node.labels)), 
    node.labels, names(node.labels))
  names(node.labels) <- ifelse(names(node.labels) == "",
    node.labels, names(node.labels))
  idx_df <- .label_idx(node.labels, nodes)
  if(any(is.na(idx_df$idx))){
    stop("All 'node.labels' should be annotated in the 'PathwaySpace' object.",
      call. = FALSE)
  }
  nodes_ft <- nodes[idx_df$idx, , drop = FALSE]
  nodes_ft$ID <- idx_df$label
  x <- y <- ID <- NULL
  ggp <- ggp + ggplot2::geom_text(
    mapping = aes(x = x, y = y, label = ID), 
    data = nodes_ft, fontface = "bold", size.unit = "mm",
    size = label.size, colour = label.color, 
    vjust="center", hjust="center")
  return(ggp)
}
.label_idx <- function(node.labels, nodes){
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
.add_labels2 <- function(ggp, nodes){
  nodes_ft <- nodes[!is.na(nodes$nodeLabel), ]
  x <- y <- nodeLabel <- NULL
  ggp <- ggp + geom_text(
    mapping = aes(x = x, y = y, label = nodeLabel), 
    data = nodes_ft, fontface = "bold",
    size = nodes_ft$nodeLabelSize, 
    colour = nodes_ft$nodeLabelColor, 
    size.unit = "pt", vjust="center", hjust="center")
  return(ggp)
}

#-------------------------------------------------------------------------------
.add_image <- function(ggp, image){
  ggp <- ggp + annotation_raster(image, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  return(ggp)
}

#-------------------------------------------------------------------------------
.custom_themes <- function(gg, theme, font.size, bg.color) {
    et1 <- ggplot2::element_text(size = 14 * font.size)
    et2 <- ggplot2::element_text(size = 12 * font.size)
    if (theme == "th3") {
        gg <- .custom_th3(gg, font.size, bg.color)
    } else if (theme == "th2") {
        gg <- .custom_th2(gg, font.size, bg.color)
    } else if (theme == "th1") {
      gg <- .custom_th1(gg, font.size, bg.color) 
    } else {
        gg <- .custom_th0(gg, font.size, bg.color)
    }
    return(gg)
}
.custom_th0 <- function(gg, font.size, bg.color) {
    et1 <- ggplot2::element_text(size = 14 * font.size)
    et2 <- ggplot2::element_text(size = 12 * font.size)
    gg <- gg + ggplot2::theme(axis.title = et1, axis.text = et2,
        legend.title = et2, legend.text = et2, legend.position = "none",
        panel.background = element_rect(fill = bg.color))
    return(gg)
}
.custom_th1 <- function(gg, font.size, bg.color) {
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
.custom_th2 <- function(gg, font.size, bg.color) {
    et1 <- ggplot2::element_text(size = 14 * font.size)
    et2 <- ggplot2::element_text(size = 12 * font.size)
    gg <- gg + ggplot2::theme_gray() + ggplot2::theme(axis.title = et1,
        axis.text = et2, legend.title = et2,
        legend.text = et2, legend.margin = margin(0, 0, 0, 0), 
        plot.margin = margin(5, 10, 0, 10), 
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.75),
        legend.position = "none",
        axis.ticks = element_line(linewidth = 0.7),
        axis.line = element_blank(), panel.border = element_blank(),
        panel.background = element_rect(fill = bg.color))
    return(gg)
}
.custom_th3 <- function(gg, font.size, bg.color) {
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
        panel.grid.major = element_line(linewidth = 0.75),
        axis.ticks = element_line(linewidth = 0.5),
        axis.line = element_blank(), panel.border = element_blank(),
        panel.background = element_rect(fill = bg.color))
    return(gg)
}


