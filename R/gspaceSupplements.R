
################################################################################
### Main constructor of GraphSpace-class objects
################################################################################
.buildGraphSpace <- function(g, mar = 0.075, verbose = TRUE) {
    # Extract nodes and attributes
    if(verbose) message("Extracting vertices...")
    nodes <- .get.nodes(g)
    nodes <- .center.nodes(nodes, mar)
    if(verbose) message("Extracting edges...")
    edges <- .get.edges(g)
    if(verbose) message("Creating a 'GraphSpace' object...")
    pars <- list(mar = mar, is.directed = igraph::is_directed(g))
    grs <- new(Class = "GraphSpace", nodes = nodes, 
        edges = edges, pars = pars, graph=g)
    return(grs)
}

################################################################################
### Get nodes in a df object
################################################################################
.get.nodes <- function(g){
    X <- igraph::V(g)$x
    Y <- igraph::V(g)$y
    vertex <- igraph::V(g)$name
    nodes <- data.frame(X = X, Y = Y, name=vertex)
    rownames(nodes) <- vertex
    nodes <- .get.node.attrs(g, nodes)
    return(nodes)
}
.get.node.attrs <- function(g, nodes){
    vatt <- .get.default.vatt()
    atts <- vertex_attr(g)[names(vatt)]
    atts <- as.data.frame(atts)
    nodes <- cbind(nodes, atts)
    return(nodes)
}

#-------------------------------------------------------------------------------
.center.nodes <- function(nodes, mar){
    if(nrow(nodes)>0){
        nodes$X <- nodes$X - mean(range(nodes$X))
        nodes$Y <- nodes$Y - mean(range(nodes$Y))
        from <- range(c(nodes$X, nodes$Y))
        to <- c(mar, 1-mar)
        nodes$X <- scales::rescale(nodes$X, from = from, to=to)
        nodes$Y <- scales::rescale(nodes$Y, from = from, to=to)
    }
    return(nodes)
}

################################################################################
### Get edges in a df object
################################################################################
.get.edges <- function(g){
    if(ecount(g)>0){
        vertex <- igraph::V(g)$name
        eatt <- .get.default.eatt(is.directed = FALSE)
        edges <- igraph::as_edgelist(g, names = FALSE)
        rownames(edges) <- colnames(edges) <- NULL
        edges <- as.data.frame(edges)
        colnames(edges) <- c("vertex1", "vertex2")
        edges$emode <- 0
        edges$name1 <- vertex[edges$vertex1]
        edges$name2 <- vertex[edges$vertex2]
        atts <- .get.eatt(g, eatt)
        if(!all(atts[,c(1,2)]==edges[,c(1,2)])){
            stop("unexpected indexing during edge attribute combination.")
        }
        edges <- cbind(edges, atts[,-c(1,2)])
        edges <- edges[order(edges$vertex1,edges$vertex2), ]
        edges <- .set.arrowtype(edges, eatt)
        edges <- .set.emode(edges)
        edges <- .adjust.arrow.length(edges)
    } else {
        edges <- .get.empty.edgedf()
    }
    return(edges)
}
.get.eatt <- function(g, eatt){
    atts <- igraph::edge_attr(g)[names(eatt)]
    atts <- as.data.frame(atts)
    e <- igraph::as_edgelist(g, names = FALSE)
    colnames(e) <- c("vertex1", "vertex2")
    atts <- cbind(e, atts)
    return(atts)
}
.set.arrowtype <- function(edges, eatt){
    a_names <- names(eatt)
    a_names <- a_names[grep("arrow",a_names)]
    a_names <- a_names[-which(a_names=="arrowType")]
    arrow1 <- arrow2 <- edges[,a_names]
    arrow1 <- .set.arrowangle1(arrow1, edges$arrowType)
    arrow2 <- .set.arrowangle2(arrow2, edges$arrowType)
    edges <- edges[, -which(colnames(edges)%in%a_names)]
    edges <- cbind(edges, arrow1, arrow2)
    return(edges)
}
.set.arrowangle1 <- function(arrow1, etype){
    arrow1$arrowAngle[etype %in% c(0, 1, -1)] <- 0
    arrow1$arrowAngle[etype %in% c(2, 3, -4)] <- 30
    arrow1$arrowAngle[etype %in% c(-2, -3, 4)] <- 90
    colnames(arrow1) <- paste0(names(arrow1), "_", 1)
    return(arrow1)
}
.set.arrowangle2 <- function(arrow2, etype){
    arrow2$arrowAngle[etype %in% c(0, 2, -2)] <- 0
    arrow2$arrowAngle[etype %in% c(1, 3, 4)] <- 30
    arrow2$arrowAngle[etype %in% c(-1, -3, -4)] <- 90
    colnames(arrow2) <- paste0(names(arrow2), "_", 2)
    return(arrow2)
}
.set.emode <- function(edges){
    emode <- abs(edges$arrowType)
    emode[emode>3] <- 3
    edges$emode <- emode
    return(edges)
}

################################################################################
### Other functions
################################################################################

#-------------------------------------------------------------------------------
.get.empty.edgedf <- function(){
    n <- numeric(); c <- character()
    edges <- data.frame(n, n, n, c, c, n, c, c, n, n, n, n, n)
    colnames(edges) <- c("vertex1","vertex2","emode","name1","name2", 
        "edgeLineWidth","edgeLineColor","edgeLineType",
        "arrowType", "arrowLength_1","arrowAngle_1",
        "arrowLength_2","arrowAngle_2")
    return(edges)
}

#-------------------------------------------------------------------------------
.adjust.arrow.length <- function(edges){
    a_theta <- 60 #default arrow angle * 2 (not implemented)
    a_theta <- a_theta / 180 * pi
    idx <- edges$arrowAngle_1==90
    if(any(idx, na.rm = TRUE)){
        l <- edges$arrowLength_1[idx]/2
        b <- sqrt( (l^2 + l^2) - (2 * l^2) * cos(a_theta))
        edges$arrowLength_1[idx] <- b + edges$edgeLineWidth[idx]/4
    }
    idx <- edges$arrowAngle_2==90
    if(any(idx, na.rm = TRUE)){
        l <- edges$arrowLength_2[idx]/2
        b <- sqrt( (l^2 + l^2) - (2 * l^2) * cos(a_theta))
        edges$arrowLength_2[idx] <- b + edges$edgeLineWidth[idx]/4
    }
    return(edges)
}

#-------------------------------------------------------------------------------
.get.exy <- function(gxy, edges){
    exy <- data.frame(
        x1 = gxy[edges$vertex1,"X"], 
        x2 = gxy[edges$vertex2,"X"], 
        y1 = gxy[edges$vertex1,"Y"], 
        y2 = gxy[edges$vertex2,"Y"])
    edges <- cbind(edges, exy)
    return(edges)
}
