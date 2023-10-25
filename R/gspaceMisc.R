
################################################################################
### Documentation for some 'toy' datasets
################################################################################
#' @title Toy 'igraph' objects
#'
#' @description Small 'igraph' objects used for workflow demonstrations.
#' All graphs include 'x', 'y', and 'name' vertex attributes.
#'
#' @format igraph
#'
#' @usage data(gtoy1)
#'
#' @source This package.
#'
#' @docType data
#' @keywords gtoys
#' @name gtoys
#' @aliases gtoy1 gtoy2
#' @return A pre-processed igraph object.
#' @examples
#' data(gtoy1)
#' data(gtoy2)
NULL

#-------------------------------------------------------------------------------
igraph.from.gspace <- function(g){
  g <- .gspace2igraph_node(g)
  g <- .gspace2igraph_edge(g)
  return(g)
}

#-------------------------------------------------------------------------------
.gspace2igraph_node <- function(g){
  vnames <- vertex_attr_names(g)
  if("nodeSize"%in%vnames){
    if(is.numeric(V(g)$nodeSize)){
      V(g)$size <- as.numeric(V(g)$nodeSize)
      g <- delete_vertex_attr(g, "nodeSize")
    }
  }
  if("nodeShape"%in%vnames){
    shapes <- .get.igraph.shapes()
    V(g)$shape <- names(shapes[V(g)$nodeShape])
    g <- delete_vertex_attr(g, "nodeShape")
  }
  if("nodeColor"%in%vnames){
    V(g)$color <- V(g)$nodeColor
    g <- delete_vertex_attr(g, "nodeColor")
  }
  if("nodeLineWidth"%in%vnames){
    V(g)$frame.width <- as.numeric(V(g)$nodeLineWidth)
    g <- delete_vertex_attr(g, "nodeLineWidth")
  }
  if("nodeLineColor"%in%vnames){
    V(g)$frame.color <- V(g)$nodeLineColor
    g <- delete_vertex_attr(g, "nodeLineColor")
  }
  if("nodeLabel"%in%vnames){
    V(g)$label <- V(g)$nodeLabel
    g <- delete_vertex_attr(g, "nodeLabel")
  }
  if("nodeLabelSize"%in%vnames){
    V(g)$label.cex <- V(g)$nodeLabelSize
    g <- delete_vertex_attr(g, "nodeLabelSize")
  }
  if("nodeLabelColor"%in%vnames){
    V(g)$label.color <- V(g)$nodeLabelColor
    g <- delete_vertex_attr(g, "nodeLabelColor")
  }
  return(g)
}

#-------------------------------------------------------------------------------
.get.igraph.shapes <- function(){
  msg <- paste0("only 'square' and 'circle' shapes are ",
    "available for transformation.")
  warning(msg)
  square <- seq(0,25)
  names(square) <- rep("square", length(square))
  circle <- c(1,8,10,17,19,20,21)
  names(circle) <- rep("circle", length(circle))
  square <- square[!square%in%circle]
  shapes <- sort(c(square,circle))
  return(shapes)
}

#-------------------------------------------------------------------------------
.gspace2igraph_edge <- function(g){
  enames <- edge_attr_names(g)
  if("edgeLineWidth"%in%enames){
    E(g)$width <- as.numeric(E(g)$edgeLineWidth)
    g <- delete_edge_attr(g, "edgeLineWidth")
  }
  if("edgeLineColor"%in%enames){
    E(g)$frame.color <- E(g)$edgeLineColor
    g <- delete_edge_attr(g, "edgeLineColor")
  }
  if("edgeLineType"%in%enames){
    E(g)$lty <- E(g)$edgeLineType
    g <- delete_edge_attr(g, "edgeLineType")
  }
  if("arrowLength"%in%enames){
    E(g)$arrow.size <- as.numeric(E(g)$arrowLength)
    g <- delete_edge_attr(g, "arrowLength")
  }
  if("arrowAngle"%in%enames){
    msg <- c("'arrowAngle' not supported.")
    warning(msg)
  }
  if("arrowType"%in%enames){
    msg <- c("'arrowType' not supported.")
    warning(msg)
  }
  if("arrowDirection"%in%enames){
    if(is.numeric(E(g)$arrowMode)){
      if(any(E(g)$arrowDirection<0) || 
          any(E(g)$arrowDirection>3)){
        msg <- c("'--|' arrows not supported.")
        warning(msg)
      }
      E(g)$arrowDirection <- abs(E(g)$arrowDirection)
      E(g)$arrowDirection[E(g)$arrowDirection>3] <- 3
      E(g)$arrow.mode <- E(g)$arrowDirection
    } else {
      arrows <- .get.arrowTypes(is.dir = FALSE)
      arrows <- arrows[E(g)$arrowDirection]
      arrows[is.na(arrows)] <- 0
      if(any(arrows<0) || any(arrows>3)){
        msg <- c("'--|' arrows not supported.")
        warning(msg)
      }
      arrows[] <- abs(arrows) 
      arrows[arrows>3] <- 3
      E(g)$arrow.mode<- arrows
    }
    g <- delete_edge_attr(g, "arrowDirection")
  }
  return(g)
}

