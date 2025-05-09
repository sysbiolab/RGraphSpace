
################################################################################
### Main constructor of GraphSpace-class objects
################################################################################
.buildGraphSpace <- function(g, layout, mar = 0.075, image = NULL, 
    verbose = TRUE) {
    
    gg <- .validate.igraph(g, layout, verbose)

    if(verbose) message("Extracting vertices...")
    nodes <- .get.nodes(gg)
    if(is.null(image)){
        image <- array()
        nodes <- .center.nodes(nodes, mar)
    } else {
        if(is.raster(image)) image <- as.matrix(image)
        if(verbose) message("Setting graph coordinates to image space...")
        xlim <- c(1, ncol(image))
        ylim <- c(1, nrow(image))
        xr <- range(nodes$X)
        yr <- range(nodes$Y)
        if( (xr[1] < xlim[1]) || (xr[2] > xlim[2]) ){
            stop("Graph coordinates outside image dimensions.", call. = FALSE)
        }
        if( (yr[1] < ylim[1]) || (yr[2] > ylim[2]) ){
            stop("Graph coordinates outside image dimensions.", call. = FALSE)
        }
        nodes <- .frame.nodes(nodes, xlim, ylim)
        if(verbose) message("--'mar' will be set to 0.")
        mar <- 0
    }

    if(verbose) message("Extracting edges...")
    if (igraph::is_directed(gg)) {
        edges <- .get.directed.edges(gg)
    } else {
        edges <- .get.edges(gg)
    }

    if(verbose) message("Creating a 'GraphSpace' object...")
    pars <- list(is.directed = is_directed(gg), mar = mar)
    gs <- new(Class = "GraphSpace", nodes = nodes, edges = edges, 
        graph=gg, image = image, pars = pars, misc = list(igraph = g))
    return(gs)
}

################################################################################
### Get nodes in a df object
################################################################################

#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
.frame.nodes <- function(nodes, xlim, ylim){
    if(nrow(nodes)>0){
        to <- c(0, 1)
        nodes$X <- scales::rescale(nodes$X, from = xlim, to=to)
        nodes$Y <- scales::rescale(nodes$Y, from = ylim, to=to)
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
        atts <- .get.eatt(g)
        if(!all(atts[,c(1,2)]==edges[,c(1,2)])){
            stop("unexpected indexing during edge attribute combination.", 
                call. = FALSE)
        }
        edges <- cbind(edges, atts[,-c(1,2)])
        edges <- edges[order(edges$vertex1,edges$vertex2), ]
        edges <- .set.arrowangle(edges)
        edges <- .set.emode(edges)
        edges <- .adjust.arrow.length(edges)
    } else {
        edges <- .get.empty.edgedf()
    }
    return(edges)
}
.get.eatt <- function(g){
    a_names <- names(.get.default.eatt())
    atts <- igraph::edge_attr(g)[a_names]
    atts <- as.data.frame(atts)
    e <- igraph::as_edgelist(g, names = FALSE)
    colnames(e) <- c("vertex1", "vertex2")
    atts <- cbind(e, atts)
    return(atts)
}
.set.arrowangle <- function(edges){
    a_names <- names(.get.default.eatt())
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
### Get undirected edges in a df object
################################################################################
.get.directed.edges <- function(g) {
    if (ecount(g) > 0) {
        vertex <- igraph::V(g)$name
        E(g)$emode <- 1
        E(g)$emode[igraph::which_mutual(g)] <- 3
        e <- emode <- .adjacency(g, attr = "emode")
        bl <- lower.tri(emode) & emode == 3
        emode[bl] <- 0
        edges <- arrayInd(seq_len(prod(dim(emode))), dim(emode), 
            useNames = TRUE)
        edges <- as.data.frame(edges)
        colnames(edges) <- c("vertex1", "vertex2")
        edges$emode <- as.numeric(emode)
        edges$name1 <- vertex[edges$vertex1]
        edges$name2 <- vertex[edges$vertex2]
        edges$e <- as.numeric(e > 0)
        eid <- e; eid[,] <- 0
        ut <- upper.tri(eid)
        eid[ut] <- seq_len(sum(ut))
        eid <- t(eid)
        eid[ut] <- seq_len(sum(ut))
        edges$eid <- as.numeric(eid)
        edges$ut <- as.numeric(upper.tri(e))
        edges$lt <- as.numeric(lower.tri(e))
        atts <- .extract.directed.att(g)
        if (!all(atts[, c(1, 2)] == edges[, c(1, 2)])) {
            stop("unexpected indexing during edge attribute combination.", 
                call. = FALSE)
        }
        edges <- cbind(edges, atts[, -c(1, 2)])
        eid <- unique(edges$eid[edges$e > 0])
        edges <- edges[edges$eid %in% eid, ]
        edges <- edges[order(edges$eid), ]
        rownames(edges) <- NULL
        edges <- .set.arrowtype.dir(edges)
        edges <- .set.arrowangle(edges)
        edges <- .set.emode(edges)
        edges <- .adjust.arrow.length(edges)
    } else {
        edges <- .get.empty.edgedf()
    }
    return(edges)
}
.set.arrowtype.dir <- function(edges, a_name = "arrowType") {
    # Flip ut/lt from single-edge arrows; this
    # for collecting arrows from the same mtx side
    idx <- which(edges$emode == 1 & edges$lt == 1)
    if (length(idx) > 0) {
        for (i in idx) {
            ii <- which(edges$eid == edges$eid[i])
            edges[ii, c("ut", "lt")] <- edges[ii, c("lt", "ut")]
        }
    }
    # collect left-side arrows
    arrow1 <- edges[edges$lt == 1, a_name]
    arrow1[is.na(arrow1)] <- 0
    # collect right-side arrows
    arrow2 <- edges[edges$ut == 1, a_name]
    arrow2[is.na(arrow2)] <- 0
    # get single-edge assigments
    edges <- edges[, -which(colnames(edges) %in% a_name)]
    edges <- edges[edges$e == 1, ]
    eid <- sort(unique(edges$eid))
    edges <- edges[order(-edges$ut, edges$eid), ]
    edges <- edges[match(eid, edges$eid), ]
    # add arrows and remove intermediate columns
    edges <- .merge.arrowtypes.dir(edges, arrow1, arrow2)
    edges <- edges[, -which(colnames(edges) %in%
            c("e", "eid", "ut", "lt"))]
    return(edges)
}
.merge.arrowtypes.dir <- function(edges, arrow1, arrow2) {
    ##  0 = "---", 1 = "-->",  2 = "<--",  3 = "<->",  4 = "|->",
    ## -1 = "--|", -2 = "|--", -3 = "|-|", -4 = "<-|",
    atypes <- c(0, 1, 2, 3, 4, -1, -2, -3, -4)
    names(atypes) <- c("00","01","10","11","-11","0-1","-10","-1-1","1-1")
    arrowType <- paste0(format(arrow1, digits = 1, trim = TRUE),
        format(arrow2, digits = 1, trim = TRUE))
    edges$arrowType <- as.numeric(atypes[arrowType])
    return(edges)
}
.extract.directed.att <- function(g) {
    # e <- igraph::as_adjacency_matrix(g, sparse = FALSE)
    e <- .adjacency(g)
    atts <- arrayInd(seq_len(prod(dim(e))), dim(e), useNames = TRUE)
    atts <- as.data.frame(atts)
    colnames(atts) <- c("vertex1", "vertex2")
    atts$e <- as.numeric(e)
    a_names <- igraph::edge_attr_names(g)
    ne <- e == 0
    for (at in a_names) {
        # x <- igraph::as_adjacency_matrix(g, sparse = FALSE, attr = at)
        x <- .adjacency(g, attr = at)
        x[ne] <- NA
        if (is.numeric(x)) {
            atts[[at]] <- as.numeric(x)
        } else if (is.character(x)) {
            atts[[at]] <- as.character(x)
        }
    }
    rownames(atts) <- NULL
    atts <- atts[, c("vertex1", "vertex2", a_names)]
    return(atts)
}
# ..this is a fix for 'as_adjacency_matrix', when 'attr' is character
.adjacency <- function(graph, attr = NULL) {
    if(is.null(attr)){
        exattr <- rep(1, ecount(graph))
    } else {
        exattr <- edge_attr(graph, as.character(attr))
    }
    if (is.logical(exattr)) {
        res <- matrix(FALSE, nrow = vcount(graph), ncol = vcount(graph))
    } else if (is.numeric(exattr)) {
        res <- matrix(0, nrow = vcount(graph), ncol = vcount(graph))
    } else {
        res <- matrix(NA, nrow = vcount(graph), ncol = vcount(graph))
    }
    e <- igraph::ends(graph, seq_len(ecount(graph)), names = FALSE)
    res[e] <- exattr
    if (!is_directed(graph)) {
        res[e[,c(2,1)]] <- exattr
    }
    colnames(res) <- rownames(res) <- V(graph)$name
    return(res)
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
