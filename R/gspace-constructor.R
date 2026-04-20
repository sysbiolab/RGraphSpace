
################################################################################
### Main constructor of GraphSpace-class objects
################################################################################
.buildGraphSpace <- function(g, layout = NULL, verbose = TRUE) {
    
    gg <- .validate_igraph(g, layout, verbose)
    nodes <- .get_nodes(gg)
    edges <- .get_edges(gg)
    
    if(verbose) message("Creating a 'GraphSpace' object...")
    pars <- list(is.directed = is_directed(gg), 
        is.normalized = FALSE, image.layer = FALSE)
    gs <- new(Class = "GraphSpace", 
        nodes = nodes, 
        edges = edges, 
        graph = gg, 
        image = as.raster(matrix()), 
        pars = pars, 
        misc = list(g = g))
    
    return(gs)
    
}

################################################################################
### Get nodes and edges in a df object
################################################################################
.get_nodes <- function(gg){
    lt <- vertex_attr(gg)
    nodes <- data.frame(row.names = seq_along(lt[[1]]))
    for(nm in names(lt)){
        nodes[[nm]] <- lt[[nm]]
    }
    vertex <- seq_len(igraph::vcount(gg))
    nodes <- cbind(vertex = vertex, nodes)
    rownames(nodes) <- nodes$name
    return(nodes)
}
.get_edges <- function(gg){
    if (igraph::is_directed(gg)) {
        edges <- .get_directed_edges(gg)
    } else {
        edges <- .get_undirected_edges(gg)
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
        edges <- cbind(edges, atts[,-c(1,2), drop = FALSE])
        edges <- edges[order(edges$vertex1,edges$vertex2), ]
        idx <- colnames(edges) %in% names(.get_empty_edgedf())
        edges <- edges[, c(which(idx), which(!idx))]
        rownames(edges) <- NULL
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
        edges <- cbind(edges, atts[, -c(1, 2), drop=FALSE])
        eid <- unique(edges$eid[edges$e > 0])
        edges <- edges[edges$eid %in% eid, ]
        edges <- edges[order(edges$eid), ]
        rownames(edges) <- NULL
        edges <- .set_arrowtype_dir(edges)
        idx <- colnames(edges) %in% names(.get_empty_edgedf())
        edges <- edges[, c(which(idx), which(!idx))]
        rownames(edges) <- NULL
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

.get_gs_edges <- function(gs){
    nodes <- gs@nodes
    edges <- gs@edges
    coord <- data.frame(
        x = nodes[edges$vertex1, "x"],
        y = nodes[edges$vertex1, "y"],
        xend = nodes[edges$vertex2, "x"],
        yend = nodes[edges$vertex2, "y"]
        )
    edges <- cbind(coord, edges)
    return(edges)
}

.get_emode <- function(arrow_type){
    emode <- abs(arrow_type)
    emode[emode>3] <- 3
    return(emode)
}

