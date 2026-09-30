
################################################################################
### Main constructor of GraphSpace-class objects
################################################################################
.buildGraphSpace <- function(g, layout = NULL, simplify = TRUE, verbose = TRUE) {
  
  if (verbose) rlang::inform("Validating the 'igraph' object...")
  
  # Warn and drop list-valued EDGE attributes (not supported)
  g <- .drop_list_edge_attrs(g)
  
  # Capture + remove list-valued VERTEX attributes ...
  vlists <- .extract_list_vertex_attrs(g)
  g <- .drop_list_vertex_attrs(g)
  
  gg <- .validate_igraph(g, layout, simplify, verbose)
  edges <- .build_edges(gg)
  nodes <- .build_nodes(gg)
  
  # Reattach captured list columns to @nodes
  # It will exist only on table for optimization
  nodes <- .attach_list_cols(nodes, vlists, key = igraph::V(gg)$name)
  
  # Capture geometries
  coords <- nodes[, c("x", "y")]
  geom_cols <- .gs_geometry_cols(nodes)
  for (col in geom_cols) {
    coords[[col]] <- nodes[[col]]
  }
  
  if(verbose) rlang::inform("Creating a 'GraphSpace' object...")
  instance_id <- .generate_gs_uuid()
  pars <- list(
    scale.factor = 1,
    is.directed = igraph::is_directed(gg), 
    is.simplified = simplify,
    is.normalized = FALSE, 
    image.space = FALSE
  )
  gs <- new(Class = "GraphSpace", 
    nodes = nodes, 
    edges = edges, 
    graph = gg,
    coords = coords,
    pars = pars,
    uuid = instance_id
  )
  
  gs
  
}

#-------------------------------------------------------------------------------
.gs_geometry_cols <- function(df) {
  if (ncol(df) == 0) return(character(0))
  names(df)[vapply(df, inherits, logical(1), what = "sfc")]
}

#-------------------------------------------------------------------------------
# Warn and drop list-valued edge attributes; edge list attributes are not
# supported. Returns the graph with those attributes removed.
.drop_list_edge_attrs <- function(g) {
  all <- igraph::edge_attr(g)
  lst <- names(all)[vapply(all, is.list, logical(1))]
  if (length(lst) > 0) {
    rlang::warn(c(
      "!" = sprintf("List-valued edge attribute(s) dropped: %s.",
        .gs_preview(lst)),
      "i" = "Only atomic edge attributes are retained during construction.",
      "*" = "To work with list attributes, set it after construction via 'gs_edge_attr()'."
    ))
    for (a in lst) g <- igraph::delete_edge_attr(g, a)
  }
  g
}

#-------------------------------------------------------------------------------
# Capture list-valued vertex attributes as a named list of columns.
# Records the pre-validation name/order for realignment.
.extract_list_vertex_attrs <- function(g) {
  all <- igraph::vertex_attr(g)
  is_lst <- vapply(all, is.list, logical(1))
  if (!any(is_lst)) return(NULL)
  cols <- all[is_lst]
  attr(cols, "key") <- if ("name" %in% names(all)) all[["name"]] else NULL
  cols
}

#-------------------------------------------------------------------------------
# Remove list-valued vertex attributes from the graph.
.drop_list_vertex_attrs <- function(g) {
  all <- igraph::vertex_attr(g)
  lst <- names(all)[vapply(all, is.list, logical(1))]
  for (a in lst) g <- igraph::delete_vertex_attr(g, a)
  g
}

#-------------------------------------------------------------------------------
# Reattach captured list columns to the nodes data.frame, aligned by index
.attach_list_cols <- function(nodes, vlists, key) {
  
  if (is.null(vlists)) return(nodes)
  
  pre_key <- attr(vlists, "key")
  lens <- lengths(vlists)
  bad  <- lens != nrow(nodes)
  if (any(bad)) {
    rlang::warn(c(
      "!" = "Could not attach list-valued vertex attribute(s); dropping them.",
      "*" = sprintf("Affected attribute(s): %s.", .gs_preview(names(vlists)[bad])),
      "i" = sprintf("Length differs from node count (%d): %s.",
        nrow(nodes),
        paste(sprintf("%s=%d", names(vlists)[bad], lens[bad]), collapse = ", "))
    ))
    vlists <- vlists[!bad]
    if (length(vlists) == 0) return(nodes)
  }
  
  idx <- if (!is.null(pre_key)) match(key, pre_key) else NULL
  if (!is.null(idx) && !anyNA(idx)) {
    # name-aligned
    for (nm in names(vlists)) nodes[[nm]] <- vlists[[nm]][idx]
  } else {
    # positional fallback: validation preserves vertex order
    for (nm in names(vlists)) nodes[[nm]] <- vlists[[nm]]
  }
  nodes
}

################################################################################
### Get nodes and edges in a df object
################################################################################
.build_nodes <- function(gg){
  lt <- igraph::vertex_attr(gg)
  n <- igraph::vcount(gg)
  nodes <- data.frame(row.names = seq_len(n) )
  for(nm in names(lt)){
    nodes[[nm]] <- lt[[nm]]
  }
  nodes <- cbind(vertex = seq_len(n), nodes)
  rownames(nodes) <- nodes$name
  nodes
}
.build_edges <- function(gg){
  
  # Entry point for directed and undirected graphs.
  # Preserve the original edge order from 
  # igraph::as_edgelist(gg)
  edges <- .get_edgelist(gg)
  
  # Post-processing only: curve_weight, is_multiple and is_loop 
  # are derived from graph structure, not real graph attributes,
  # and are never written back to @graph.
  edges$curve_weight <- .get_curve_weight(edges$vertex1, edges$vertex2, 
    igraph::is_directed(gg))
  edges$is_multiple <- .get_is_multiple(edges$vertex1, edges$vertex2)
  edges$is_loop <- edges$vertex1 == edges$vertex2
  edges
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
    idx <- colnames(edges) %in% names(.get_empty_edgedf())
    edges <- edges[, c(which(idx), which(!idx))]
    rownames(edges) <- NULL
  } else {
    edges <- .get_empty_edgedf()
  }
  edges
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
  atts
}

.get_empty_edgedf <- function(){
  n <- numeric(); c <- character()
  edges <- data.frame(n, n, c, c, c, c, n, n, n)
  colnames(edges) <- c("vertex1","vertex2", "name1", "name2", 
    "edgeLineType", "edgeColor", "edgeLineWidth",
    "arrowType", "weight")
  edges
}

################################################################################
### Other functions
################################################################################

#-------------------------------------------------------------------------------
# emode: 0 none, 1 end-only, 2 start-only, 3 both. Reads the canonical token
# string; the numeric branch remains only for raw integer input at the boundary.
.get_emode <- function(arrow_type){
  if(is.numeric(arrow_type)){
    emode <- abs(arrow_type)
    emode[emode>3] <- 3
    return(emode)
  }
  tk <- .arrowtype_to_tokens(arrow_type)
  has_start <- tk[, "start"] != "-"
  has_end <- tk[, "end"] != "-"
  as.integer(has_end) + (as.integer(has_start) * 2L)
}

#-------------------------------------------------------------------------------
.gs_nodes <- function(gs){
  nodes <- gs@nodes
  nodes$away_angle <- .get_node_away_angle(nodes)
  nodes
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
  edges
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
  is_center <- away_len < layout_scale * 0.01
  is_center[is.na(is_center)] <- FALSE
  angle[is_center] <- 90
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
  is_center <- away_len < layout_scale * 0.01
  is_center[is.na(is_center)] <- FALSE
  edge_angle[is_center] <- 90
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
  
  weight
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
