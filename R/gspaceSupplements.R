
################################################################################
### Plotting adjusts for GraphSpace-class methods
################################################################################

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

