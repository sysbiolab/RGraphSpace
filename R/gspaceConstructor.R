
################################################################################
### Main constructor of GraphSpace-class objects
################################################################################
.buildGraphSpace <- function(g, mar = 0.1, image = NULL, layout = NULL,
    verbose = TRUE) {
    
    gg <- .validate_igraph(g, layout, verbose)
    
    if(verbose) message("Extracting vertices...")
    nodes <- .get_nodes(gg)
    temp <- .center_nodes(nodes, image, mar, verbose=verbose)
    nodes <- temp$nodes
    image.mar <- temp$image
    image.layer <- temp$image.layer
    
    if(verbose) message("Extracting edges...")
    edges <- .get_edges(gg)
    
    if(verbose) message("Creating a 'GraphSpace' object...")
    pars <- list(is.directed = is_directed(gg), mar = mar, 
        image.layer = image.layer)
    gs <- new(Class = "GraphSpace", nodes = nodes, edges = edges, 
        graph=gg, image = image.mar, pars = pars, 
        misc = list(g = g, image=image))
    return(gs)
}

################################################################################
### Functions for image adjusts
################################################################################

#-------------------------------------------------------------------------------
.frame_nodes <- function(nodes, image, mar){
    d <- dim(image)
    xr <- range(nodes$x)
    yr <- range(nodes$y)
    if( (xr[1] < 1) || (xr[2] > d[2]) ){
        stop("Graph coordinates outside image dimensions.", call. = FALSE)
    }
    if( (yr[1] < 1) || (yr[2] > d[1]) ){
        stop("Graph coordinates outside image dimensions.", call. = FALSE)
    }
    # adjust image and node coordinates
    res <- .crop_image(nds=nodes, img=image, mar)
    res <- .square_image(res$nodes, res$image)
    # normalize node coordinates
    d <- dim(res$image)
    res$nodes$x <- scales::rescale(res$nodes$x, from = c(1, d[2]), to = c(0, 1))
    res$nodes$y <- scales::rescale(res$nodes$y, from = c(1, d[1]), to = c(0, 1))
    return(res)
}

#-------------------------------------------------------------------------------
.crop_image <- function(nds, img, mar){
    
    d <- dim(img)
    
    # set node limits to integer
    xl <- range(nds$x)
    yl <- range(nds$y)
    xl <- c(ceiling(xl[1]), floor(xl[2]))
    yl <- c(ceiling(yl[1]), floor(yl[2]))
    nds$x <- scales::rescale(nds$x, to=xl)
    nds$y <- scales::rescale(nds$y, to=yl)
    
    # set margins
    dp <- c(yl[2]-yl[1], xl[2]-xl[1])
    m <- floor(min(dp) * mar)
    dxmar <- min( c(xl[1], d[2] - xl[2]) )
    dymar <- min( c(yl[1], d[1] - yl[2]) )
    m <- min(c(m, dxmar, dymar))
    m <- max(m, 1)
    xl <- c(xl[1] - m, xl[2] + m)
    yl <- c(yl[1] - m, yl[2] + m)
    
    # crop on flipped rows to match node y-coordinates
    img <- img[seq.int(nrow(img), 1), ]
    img <- img[seq.int(yl[1], yl[2]), seq.int(xl[1], xl[2])]
    img <- img[seq.int(nrow(img), 1), ]
    
    # set new node coordinates
    xl <- c(m + 1, ncol(img) - m)
    yl <- c(m + 1, nrow(img) - m)
    nds$x <- scales::rescale(nds$x, to=xl)
    nds$y <- scales::rescale(nds$y, to=yl)
    
    res <- list(nodes=nds, image=img)
    return(res)
} 
# adjust limits to a square window
.adjust_lim <- function(xl, yl, d){
    dx <- xl[2] - xl[1] + 1
    dy <- yl[2] - yl[1] + 1
    if(dx > dy){
        dm <- (dx - dy)/2
        yl <- c(yl[1] - ceiling(dm), yl[2] + floor(dm))
        if(yl[1] < 1) yl[1] <- 1
        if(yl[2] > d[2]) yl[2] <- d[2]
    } else if(dx < dy){
        dm <- (dy - dx)/2
        xl <- c(xl[1] - ceiling(dm), xl[2] + floor(dm))
        if(xl[1] < 1) xl[1] <- 1
        if(xl[2] > d[2]) xl[2] <- d[2]
    }
    dx <- xl[2] - xl[1] + 1
    dy <- yl[2] - yl[1] + 1
    res <- list(xl = xl, yl = yl, mlen = max(dx,dy))
    return(res)
}

#-------------------------------------------------------------------------------
.square_image <- function(nds, img ){
    d <- dim(img )
    if(d[1] > d[2]){
        n <- ceiling( (d[1] - d[2]) )/2
        img_d <- matrix(NA, nrow = d[1], ncol = d[1])
        img_d[ , seq(n + 1, n + d[2])] <- as.matrix(img)
        nds$x <- nds$x + n
        img  <- as.raster(img_d)
    } else if(d[1] < d[2]){
        n <- ceiling( (d[2] - d[1])/2 )
        img_d <- matrix(NA, nrow = d[2], ncol = d[2])
        img_d[seq(n + 1, n + d[1]), ] <- as.matrix(img)
        nds$y <- nds$y + n
        img  <- as.raster(img_d)
    }
    res <- list(nodes=nds, image=img)
    return(res)
}

#-------------------------------------------------------------------------------
.center_nodes <- function(nodes, image, mar, verbose = FALSE){
    if(is.null(image)){
        if(nrow(nodes)>0){
            nodes$x <- nodes$x - mean(range(nodes$x))
            nodes$y <- nodes$y - mean(range(nodes$y))
            from <- range(c(nodes$x, nodes$y))
            to <- c(mar, 1-mar)
            nodes$x <- scales::rescale(nodes$x, from = from, to=to)
            nodes$y <- scales::rescale(nodes$y, from = from, to=to)
        }
        temp <- list(nodes=nodes, image=as.raster(matrix()),
            image.layer = FALSE)
    } else {
        if(verbose) message("Setting graph coordinates to image space...")
        if(!is.raster(image)) image <- as.raster(image)
        if(nrow(nodes) > 0){
            temp <- .frame_nodes(nodes, image, mar)
            temp$image.layer <- TRUE
        } else {
            temp <- list(nodes=nodes, image=image, image.layer=TRUE)
        }
    }
    return(temp)
}

################################################################################
### Get nodes and edges in a df object
################################################################################
.get_nodes <- function(g){
    lt <- vertex_attr(g)
    nodes <- data.frame(row.names = seq_along(lt[[1]]))
    for(nm in names(lt)){
        nodes[[nm]] <- lt[[nm]]
    }
    vertex <- seq_len(igraph::vcount(g))
    nodes <- cbind(vertex = vertex, nodes)
    rownames(nodes) <- nodes$name
    return(nodes)
}
.get_edges <- function(g){
    if (igraph::is_directed(g)) {
        edges <- .get_directed_edges(g)
    } else {
        edges <- .get_undirected_edges(g)
    }
    return(edges)
}

################################################################################
### Get edges in a df object
################################################################################
.get_undirected_edges <- function(g){
    if(ecount(g)>0){
        vertex <- igraph::V(g)$name
        edges <- igraph::as_edgelist(g, names = FALSE)
        rownames(edges) <- colnames(edges) <- NULL
        edges <- as.data.frame(edges)
        colnames(edges) <- c("vertex1", "vertex2")
        edges$name1 <- vertex[edges$vertex1]
        edges$name2 <- vertex[edges$vertex2]
        atts <- .get_eatt(g)
        if(!all(atts[,c(1,2)]==edges[,c(1,2)])){
            stop("unexpected indexing during edge attribute combination.", 
                call. = FALSE)
        }
        edges <- cbind(edges, atts[,-c(1,2)])
        edges <- edges[order(edges$vertex1,edges$vertex2), ]
        idx <- colnames(edges) %in% names(.get_empty_edgedf())
        edges <- edges[, c(which(idx), which(!idx))]
    } else {
        edges <- .get_empty_edgedf()
    }
    return(edges)
}
.get_eatt <- function(g){
    lt <- igraph::edge_attr(g)
    atts <- data.frame(row.names = seq_along(lt[[1]]))
    for(nm in names(lt)){
        atts[[nm]] <- lt[[nm]]
    }
    e <- igraph::as_edgelist(g, names = FALSE)
    colnames(e) <- c("vertex1", "vertex2")
    atts <- cbind(e, atts)
    return(atts)
}
.get_empty_edgedf <- function(){
    n <- numeric(); c <- character()
    edges <- data.frame(n, n, c, c, c, c, n, n, n)
    colnames(edges) <- c("vertex1","vertex2", "name1", "name2", 
        "edgeLineType", "edgeLineColor", "edgeLineWidth",
        "arrowType", "weight")
    return(edges)
}

################################################################################
### Get undirected edges in a df object
################################################################################
.get_directed_edges <- function(g) {
    if (ecount(g) > 0) {
        atts <- .extract_directed_att(g)
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
        if (!all(atts[, c(1, 2)] == edges[, c(1, 2)])) {
            stop("unexpected indexing during edge attribute combination.", 
                call. = FALSE)
        }
        edges <- cbind(edges, atts[, -c(1, 2)])
        eid <- unique(edges$eid[edges$e > 0])
        edges <- edges[edges$eid %in% eid, ]
        edges <- edges[order(edges$eid), ]
        rownames(edges) <- NULL
        edges <- .set_arrowtype_dir(edges)
        idx <- colnames(edges) %in% names(.get_empty_edgedf())
        edges <- edges[, c(which(idx), which(!idx))]
    } else {
        edges <- .get_empty_edgedf()
    }
    return(edges)
}
.set_arrowtype_dir <- function(edges) {
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
    arrow1 <- edges[edges$lt == 1, "arrowType"]
    arrow1[is.na(arrow1)] <- 0
    # collect right-side arrows
    arrow2 <- edges[edges$ut == 1, "arrowType"]
    arrow2[is.na(arrow2)] <- 0
    # get single-edge assignments
    edges <- edges[edges$e == 1, ]
    eid <- sort(unique(edges$eid))
    edges <- edges[order(-edges$ut, edges$eid), ]
    edges <- edges[match(eid, edges$eid), ]
    # add arrows and remove intermediate columns
    edges <- .merge_arrowtypes_dir(edges, arrow1, arrow2)
    edges <- edges[, -which(colnames(edges) %in% 
            c("e", "eid", "ut", "lt","emode"))]
    return(edges)
}
.merge_arrowtypes_dir <- function(edges, arrow1, arrow2) {
    ##  0 = "---", 1 = "-->",  2 = "<--",  3 = "<->",  4 = "|->",
    ## -1 = "--|", -2 = "|--", -3 = "|-|", -4 = "<-|",
    atypes <- c(0, 1, 2, 3, 4, -1, -2, -3, -4)
    names(atypes) <- c("00","01","10","11","-11","0-1","-10","-1-1","1-1")
    arrowType <- paste0(format(arrow1, digits = 1, trim = TRUE),
        format(arrow2, digits = 1, trim = TRUE))
    edges$arrowType <- as.numeric(atypes[arrowType])
    return(edges)
}
.extract_directed_att <- function(g) {
    # e <- igraph::as_adjacency_matrix(g, sparse = FALSE)
    e <- .adjacency(g)
    atts <- arrayInd(seq_len(prod(dim(e))), dim(e), useNames = TRUE)
    atts <- as.data.frame(atts)
    colnames(atts) <- c("vertex1", "vertex2")
    atts$e <- as.numeric(e)
    # a_names <- names(.get.default.eatt())
    a_names <- igraph::edge_attr_names(g)
    ne <- e == 0
    for (at in a_names) {
        x <- .adjacency(g, attr = at)
        x[ne] <- NA
        if (is.data.frame(x)){
            atts[[at]] <- I(unlist(x, recursive=FALSE))
        } else {
            if (is.numeric(x)) {
                atts[[at]] <- as.numeric(x)
            } else if (is.character(x)) {
                atts[[at]] <- as.character(x)
            }
        }
    }
    rownames(atts) <- NULL
    atts <- atts[, c("vertex1", "vertex2", a_names)]
    return(atts)
}
# ..this is a fix for 'as_adjacency_matrix', when 'attr' is character
.adjacency <- function(g, attr = NULL) {
    if(is.null(attr)){
        exattr <- rep(1, ecount(g))
        x <- matrix(0, nrow = vcount(g), ncol = vcount(g))
    } else {
        exattr <- edge_attr(g, as.character(attr))
        x <- matrix(NA, nrow = vcount(g), ncol = vcount(g))
        if(is.list(exattr)) x <- as.data.frame(x)
    }
    e <- igraph::ends(g, seq_len(ecount(g)), names = FALSE)
    x[e] <- exattr
    if (!is_directed(g)) x[e[,c(2,1)]] <- exattr
    colnames(x) <- rownames(x) <- V(g)$name
    return(x)
}

################################################################################
### Other functions
################################################################################

#-------------------------------------------------------------------------------
.get_emode <- function(arrow_type){
    emode <- abs(arrow_type)
    emode[emode>3] <- 3
    return(emode)
}

#-------------------------------------------------------------------------------
.get_edge_coords <- function(gs){
    #--- Add segments
    nodes <- gs@nodes
    edges <- gs@edges
    edges$x <- nodes[edges$vertex1,"x"]
    edges$y <- nodes[edges$vertex1,"y"]
    edges$xend <- nodes[edges$vertex2,"x"]
    edges$yend <- nodes[edges$vertex2,"y"]
    #--- Estimate arrow offsets from node radius and line width
    offsets <- (nodes$nodeSize/2 * 0.01) + (nodes$nodeLineWidth * 0.01)
    #--- Add arrow offsets
    emode <- .get_emode(edges$arrowType)
    edges$offsetStart <- ifelse(emode %in% c(0,1), 0, 
        offsets[edges$vertex1])
    edges$offsetEnd <- ifelse(emode %in% c(0,2), 0, 
        offsets[edges$vertex2])
    return(edges)
}
.get_node_coords <- function(gs){
    nodes <- gs@nodes
    # Note1: nodeSize is interpreted as % of graph space; here returned for npc
    # Note2: for 'pch' in 0:25, size is about 75% of the character
    # height (see 'points()' graphics); here rectified by a 1.25 factor
    nodes$nodeSize <- nodes$nodeSize * 0.01 * 1.25
    return(nodes)
}
