
################################################################################
### Main constructor of GraphSpace-class objects
################################################################################
.buildGraphSpace <- function(g, layout = NULL, simplify = TRUE, verbose = TRUE) {
    
    gg <- .validate_igraph(g, layout, simplify, verbose)
    edges <- .get_edges(gg, simplify)
    nodes <- .get_nodes(gg)
    
    if(verbose) rlang::inform("Creating a 'GraphSpace' object...")
    instance_id <- .generate_gs_uuid()
    pars <- list(
        is.directed = igraph::is_directed(gg), 
        is.simplified = simplify,
        is.normalized = FALSE, 
        image.space = FALSE)
    gs <- new(Class = "GraphSpace", 
        nodes = nodes, edges = edges, graph = gg, 
        image = grDevices::as.raster(matrix()), 
        pars = pars, 
        misc = list(),
        uuid = instance_id
        )
    
    return(gs)
    
}

################################################################################
### Get nodes and edges in a df object
################################################################################
.get_nodes <- function(gg){
    lt <- vertex_attr(gg)
    n <- igraph::vcount(gg)
    nodes <- data.frame(row.names = seq_len(n) )
    for(nm in names(lt)){
        nodes[[nm]] <- lt[[nm]]
    }
    nodes <- cbind(vertex = seq_len(n), nodes)
    rownames(nodes) <- nodes$name
    return(nodes)
}
.get_edges <- function(gg, simplify = TRUE){
    
    if (simplify && is_simple(gg) && igraph::is_directed(gg)) {
        edges <- .get_simplified_edgelist(gg)
    } else {
        edges <- .get_edgelist(gg)
    }
    # Post-processing only: curve_weight, is_multiple and is_loop 
    # are derived from graph structure, not real graph attributes,
    # and are never written back to @graph.
    edges$curve_weight <- .get_curve_weight(edges$vertex1, edges$vertex2, 
        igraph::is_directed(gg))
    edges$is_multiple <- .get_is_multiple(edges$vertex1, edges$vertex2)
    edges$is_loop <- edges$vertex1 == edges$vertex2
    return(edges)
}

################################################################################
### Get either directed or undirected edge lists
################################################################################
.get_edgelist <- function(g){
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
            rlang::abort("unexpected indexing during edge attribute combination.")
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
### Get directed edge lists in a simplified format
################################################################################
.get_simplified_edgelist <- function(g) {
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
            rlang::abort("unexpected indexing during edge attribute combination.")
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
    ## arrow1/arrow2 are guaranteed in {-1, 0, 1} (validated upstream),
    ## so all 9 combinations below are exhaustive by construction.
    ## No empty edges will reach this point.
    atypes <- c(0, 1, 2, 3, 4, -1, -2, -3, -4)
    names(atypes) <- c("00","01","10","11","-11","0-1","-10","-1-1","1-1")
    
    ## format(..., digits = 1, trim = TRUE) on -1/0/1 always yields "-1"/"0"/"1"
    ## (no decimal point, no leading/trailing whitespace), so paste0() produces
    ## exactly one of the 9 keys in names(atypes) above -- never a partial or
    ## malformed key.
    arrowType <- paste0(format(arrow1, digits = 1, trim = TRUE),
        format(arrow2, digits = 1, trim = TRUE))
    
    ## Named-vector lookup: atypes[arrowType] returns NA for any unmatched key.
    ## Given the guarantees above, this never happens -- see comments at top.
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
            } else if (is.logical(x)) {
                atts[[at]] <- as.logical(x)
            } else {
                atts[[at]] <- as.vector(x)
            }
        }
    }
    a_names_present <- a_names[a_names %in% colnames(atts)]
    atts <- atts[, c("vertex1", "vertex2", a_names_present)]
    rownames(atts) <- NULL
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
.gs_nodes <- function(gs){
    nodes <- gs@nodes
    nodes$away_angle <- .get_node_away_angle(nodes)
    return(nodes)
}

#-------------------------------------------------------------------------------
.gs_edges <- function(gs){
    nodes <- .gs_nodes(gs)
    edges <- gs@edges
    coord <- data.frame(
        x = nodes[edges$vertex1, "x"],
        y = nodes[edges$vertex1, "y"],
        xend = nodes[edges$vertex2, "x"],
        yend = nodes[edges$vertex2, "y"]
        )
    n_offsets <- nodes[["nodeSize"]]
    coord$offset_start <- n_offsets[edges$vertex1]
    coord$offset_end <- n_offsets[edges$vertex2]
    edges$away_angle <- .get_edge_away_angle(coord, nodes)
    gs_id <- attr(edges, "gs_id")
    edges <- cbind(coord, edges)
    attr(edges, "gs_id") <- gs_id
    return(edges)
}

#-------------------------------------------------------------------------------
# Node-level "away from centroid" angle (degrees). 
.get_node_away_angle <- function(nodes){
    cx <- mean(nodes$x, na.rm = TRUE)
    cy <- mean(nodes$y, na.rm = TRUE)
    layout_scale <- sqrt(stats::var(nodes$x, na.rm = TRUE) +
            stats::var(nodes$y, na.rm = TRUE))
    if (nrow(nodes) < 2 || !is.finite(layout_scale) || layout_scale == 0) {
        return(rep(90, nrow(nodes)))
    }
    away_x <- nodes$x - cx
    away_y <- nodes$y - cy
    away_len <- sqrt(away_x^2 + away_y^2)
    angle <- atan2(away_y, away_x) * 180 / pi
    angle[away_len < layout_scale * 0.01] <- 90
    angle
}

#-------------------------------------------------------------------------------
# Edge-level "away from centroid" angle (degrees). 
.get_edge_away_angle <- function(coord, nodes){
    cx <- mean(nodes$x, na.rm = TRUE)
    cy <- mean(nodes$y, na.rm = TRUE)
    layout_scale <- sqrt(stats::var(nodes$x, na.rm = TRUE) +
            stats::var(nodes$y, na.rm = TRUE))
    if (nrow(nodes) < 2 || !is.finite(layout_scale) || layout_scale == 0) {
        return(rep(90, nrow(nodes)))
    }
    mid_x <- (coord$x + coord$xend) / 2
    mid_y <- (coord$y + coord$yend) / 2
    away_x <- mid_x - cx
    away_y <- mid_y - cy
    away_len <- sqrt(away_x^2 + away_y^2)
    edge_angle <- atan2(away_y, away_x) * 180 / pi
    edge_angle[away_len < layout_scale * 0.01] <- 90
    edge_angle
}

#-------------------------------------------------------------------------------
.get_is_multiple <- function(vertex1, vertex2){
    lo <- pmin(vertex1, vertex2)
    hi <- pmax(vertex1, vertex2)
    key <- paste(lo, hi, sep = "_")
    group_size <- table(key)
    as.logical(group_size[key] > 1)
}

#-------------------------------------------------------------------------------
# Computes a per-edge "weight" in [-1, 1] for automatically distributing
# curvature among parallel edges and self-loops, so that geom_edgespace()
# can later just multiply this by the user's `curve` value at render
# time (curve_final <- curve_param * curve_weight) with no further
# graph-level computation.
.get_curve_weight <- function(vertex1, vertex2, is_directed){
    
    n <- length(vertex1)
    weight <- numeric(n)
    
    is_loop <- vertex1 == vertex2
    lo <- pmin(vertex1, vertex2)
    hi <- pmax(vertex1, vertex2)
    key <- paste(lo, hi, sep = "_")
    
    # split() builds the full key -> row-index map in one pass (hash-based,
    # O(e) average), avoiding the O(e^2) worst case of calling which(key == k)
    # inside a loop over unique pairs. The loop body is otherwise unchanged.
    idx_by_key <- split(seq_len(n), key)
    
    for (idx in idx_by_key) {
        if (is_loop[idx[1]]) {
            weight[idx] <- .fan_onesided(length(idx))
        } else if (!is_directed) {
            weight[idx] <- .fan_symmetric(length(idx))
        } else {
            is_fwd <- vertex1[idx] == lo[idx]
            idx_fwd <- idx[is_fwd]
            idx_bwd <- idx[!is_fwd]
            if (length(idx_fwd) == 0 || length(idx_bwd) == 0) {
                weight[idx] <- .fan_symmetric(length(idx))
            } else {
                weight[idx_fwd] <- .fan_onesided(length(idx_fwd))
                weight[idx_bwd] <- .fan_onesided(length(idx_bwd))
            }
        }
    }
    
    return(weight)
}

# i/n for i = 1..n: ascending, NEVER zero. Used for one side of a
# directed pair, and (via .fan_split) for one half of a self-loop group.
.fan_onesided <- function(n){
    seq_len(n) / n
}

# n == 1 -> 1 (the user's curve value applies exactly, since there's
# nothing to disambiguate from)
.fan_symmetric <- function(n){
    if (n == 1) return(1)
    seq(-1, 1, length.out = n)
}


