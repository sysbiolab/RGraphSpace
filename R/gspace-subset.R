
################################################################################
### Filtering methods for GraphSpace objects
################################################################################

#-------------------------------------------------------------------------------
#' @title Filter nodes and edges in a GraphSpace object
#'
#' @description
#' \code{gs_subset_nodes()} retains a subset of nodes and automatically
#' removes any edge whose endpoint is no longer present.
#'
#' \code{gs_subset_edges()} retains a subset of edges without modifying
#' the node set.
#'
#' @param x A \code{\link{GraphSpace}} object.
#' @param i A filter specification. Accepted forms:
#' \itemize{
#'   \item A \strong{character} vector of node names
#'     (\code{gs_subset_nodes()} only; edges are identified by
#'     integer position or predicate, not by name).
#'   \item An \strong{integer} vector of positional indices into the
#'     node or edge table.
#'   \item A \strong{logical} vector whose length must match the number
#'     of nodes or edges, respectively.
#'   \item An \strong{unquoted predicate} evaluated against the node or
#'     edge data frame using data masking, such as \code{nodeSize > 5}
#'     or \code{weight > 0.5}. Column names from the relevant table are
#'     available directly as variables inside the expression.
#' }
#'
#' @details
#'
#' \strong{Node filtering} preserves the normalized coordinate state.
#' Coordinates for surviving nodes remain in their current space
#' (\code{[0, 1]} if normalized, raw coordinates otherwise), so
#' \code{\link{normalizeGraphSpace}} does not need to be re-run.
#' The \code{@graph}, \code{@fdata}, \code{@nodes}, and \code{@edges}
#' slots are all updated consistently. The \code{@canvas} and
#' background image are not modified.
#'
#' \strong{Edge filtering} leaves the node set and the layout entirely
#' intact. Because removing an edge from a group of parallel edges
#' invalidates the derived attributes \code{curve_weight},
#' \code{is_multiple}, and \code{is_loop} for the remaining members of
#' that group, the full edge table is recomputed from \code{@graph}
#' after deletion.
#'
#' \strong{Note on parallel edges}: in non-simplified graphs containing
#' parallel edges between the same vertex pair, integer or logical
#' indexing is the most reliable approach. A predicate expression that
#' matches a shared attribute (such as \code{edgeColor}) will match
#' all parallel instances simultaneously, which is usually the intended
#' behavior.
#'
#' @return A \code{\link{GraphSpace}} object with the selected subset
#' of nodes or edges.
#'
#' @seealso
#' \code{\link{cropGraphSpace}}, \code{\link{gs_nodes}},
#' \code{\link{gs_edges}}, \code{\link{normalizeGraphSpace}}
#'
#' @examples
#' library(RGraphSpace)
#' library(igraph)
#'
#' # Create a directed star graph with numeric attributes
#' g <- make_star(10, mode = "out")
#' V(g)$nodeSize <- runif(vcount(g), 1, 10)
#' E(g)$weight   <- runif(ecount(g), 0, 1)
#' gs <- GraphSpace(g)
#' gs <- normalizeGraphSpace(gs)
#'
#' #--- gs_subset_nodes examples ---
#'
#' # By node name (character vector)
#' gs2 <- gs_subset_nodes(gs, c("n1", "n2", "n3"))
#'
#' # By integer position
#' gs2 <- gs_subset_nodes(gs, 1:5)
#'
#' # By predicate (data masking against @nodes columns)
#' gs2 <- gs_subset_nodes(gs, nodeSize > 5)
#'
#' # By pre-evaluated logical vector
#' keep <- gs$nodeSize > 5
#' gs2  <- gs_subset_nodes(gs, keep)
#'
#' # Combining with pipes
#' gs2 <- gs |>
#'   gs_subset_nodes(nodeSize > 5) |>
#'   gs_subset_edges(weight > 0.3)
#'
#' #--- gs_subset_edges examples ---
#'
#' # By predicate on an edge attribute
#' gs3 <- gs_subset_edges(gs, weight > 0.5)
#'
#' # By endpoint names: name1 and name2 are columns in @edges and
#' # can be used directly inside any predicate expression
#' gs3 <- gs_subset_edges(gs, name1 == "n1")
#' gs3 <- gs_subset_edges(gs, name2 == "n1")
#'
#' # Combining endpoint and attribute conditions
#' gs3 <- gs_subset_edges(gs, name1 == "n1" & weight > 0.5)
#'
#' # By integer position
#' gs3 <- gs_subset_edges(gs, 1:3)
#'
#' # By logical vector
#' gs3 <- gs_subset_edges(gs, gs_edges(gs)$weight > 0.5)
#'
#' @name gs_subset
#' @aliases gs_subset_nodes gs_subset_edges
NULL

#-------------------------------------------------------------------------------
#' @rdname gs_subset
#' @export
gs_subset_nodes <- function(x, i) {
  
  if (!inherits(x, "GraphSpace")) {
    rlang::abort("'x' must be a GraphSpace object.")
  }
  
  if (gs_vcount(x) == 0) {
    rlang::warn("The 'GraphSpace' object has no nodes to filter.")
    return(invisible(x))
  }
  
  if (missing(i)) {
    return(x)
  }
  
  i_quo <- rlang::enquo(i)
  fvars <- all.vars(rlang::quo_get_expr(i_quo))
  nodes <- gs_nodes(x, vars = fvars)
  idx <- .resolve_gs_index(i_quo, data = nodes, what = "node")
  
  # Nothing to do when every node survives
  if (setequal(idx, seq_len(nrow(nodes)))) {
    return(x)
  }
  
  if (length(idx) == 0L) {
    rlang::warn(c(
      "No nodes matched the filter expression.",
      "i" = "The returned object contains no nodes or edges."
    ))
  }
  
  nodes_kept <- nodes[idx, , drop = FALSE]
  
  # .trim_graph_space() prunes @edges, remaps vertex indices,
  # updates @graph, and trims @fdata — all in one consistent pass.
  # @pars (including is.normalized) and @canvas are intentionally
  # left untouched; surviving nodes retain their current coordinates.
  x <- .trim_graph_space(x, nodes_kept)
  
  validObject(x)
  return(x)
  
}

#-------------------------------------------------------------------------------
#' @rdname gs_subset
#' @export
gs_subset_edges <- function(x, i) {

  if (!inherits(x, "GraphSpace")) {
    rlang::abort("'x' must be a GraphSpace object.")
  }

  edges <- x@edges

  if (nrow(edges) == 0L) {
    rlang::warn("The 'GraphSpace' object has no edges to filter.")
    return(invisible(x))
  }

  if (missing(i)) {
    return(x)
  }

  i_quo <- rlang::enquo(i)
  idx   <- .resolve_gs_index(i_quo, data = edges, what = "edge")

  # Nothing to do when every edge survives
  if (setequal(idx, seq_len(nrow(edges)))) {
    return(x)
  }

  if (length(idx) == 0L) {
    rlang::warn(c(
      "No edges matched the filter expression.",
      "i" = "The returned object contains no edges."
    ))
  }

  remove_idx    <- setdiff(seq_len(nrow(edges)), idx)
  edges_removed <- edges[remove_idx, , drop = FALSE]

  # Map the removed @edges rows back to igraph edge IDs and delete them.
  # For simplified directed graphs, arrowType ±3 rows map to TWO igraph
  # edges (A->B and B->A), both of which must be removed.
  igraph_ids <- .gs_get_edge_ids(x@graph, edges_removed)

  if (length(igraph_ids) > 0L) {
    x@graph <- igraph::delete_edges(x@graph, igraph_ids)
  }

  # Rebuild @edges from the updated @graph so that derived quantities
  # (curve_weight, is_multiple, is_loop) are consistent. A surviving
  # partner of a removed parallel edge must have is_multiple updated.
  x@edges <- .get_edges(x@graph, simplify = .is_simplified(x))

  validObject(x)
  return(x)

}

################################################################################
### Internal helpers
################################################################################

#-------------------------------------------------------------------------------
# Resolve the user-supplied index 'i_quo' (a captured quosure) against a node
# or edge data frame. Returns an integer vector of row positions to KEEP.
# Accepted forms (evaluated with data masking against 'data'):
#--character: matched against data[["name"]] (node table only for now)
#--logical: must have length == nrow(data); NA treated as FALSE
#--integer: unique positions in [1, nrow(data)]
#--predicate: any expression whose result is one of the above
.resolve_gs_index <- function(i_quo, data, what = "node") {

  n <- nrow(data)

  val <- tryCatch(
    rlang::eval_tidy(i_quo, data = data),
    error = function(e) {
      rlang::abort(c(
        x = sprintf(
          "Failed to evaluate the %s filter expression.", what),
        i = conditionMessage(e)
      ))
    }
  )

  #--- language: a quoted call (e.g. quote(name1 == "n1")) passed via [
  # eval_tidy returns the call object itself without further evaluation;
  # re-evaluate it against the data frame to resolve the actual result.
  if (is.language(val)) {
    val <- tryCatch(
      rlang::eval_tidy(val, data = data),
      error = function(e) {
        rlang::abort(c(
          x = sprintf(
            "Failed to evaluate the quoted %s expression.", what),
          i = conditionMessage(e)
        ))
      }
    )
  }

  #--- character: match against the 'name' identifier column
  if (is.character(val)) {

    if (what != "node") {
      rlang::abort(c(
        x = "Character indexing is not supported for edges.",
        i = "Use a logical vector, integer positions, or a predicate expression."
      ))
    }

    id_col <- data[["name"]]
    val_unique <- unique(val)
    idx <- match(val_unique, id_col)
    
    missing_ids <- setdiff(val, id_col)
    if (length(missing_ids) > 0L) {
      rlang::warn(c(
        sprintf(
          "%d node name(s) not found and will be ignored.",
          length(missing_ids)
        ),
        "i" = .gs_preview(missing_ids)
      ))
    }
    idx <- idx[!is.na(idx)]
  #--- logical: must be the same length as the table
  } else if (is.logical(val)) {

    if (length(val) != n) {
      rlang::abort(sprintf(
        "Logical filter length (%d) must equal the number of %ss (%d).",
        length(val), what, n
      ))
    }
    val[is.na(val)] <- FALSE
    idx <- which(val)

  #--- integer / numeric: positional indices
  } else if (is.numeric(val) || is.integer(val)) {

    val    <- as.integer(val)
    na_pos <- is.na(val)

    if (any(na_pos)) {
      rlang::warn(sprintf(
        "%d NA index value(s) removed.", sum(na_pos)
      ))
      val <- val[!na_pos]
    }

    if (length(val) > 0L && (any(val < 1L) || any(val > n))) {
      rlang::abort(sprintf(
        "Integer index out of range: valid positions for %ss are [1, %d].",
        what, n
      ))
    }

    idx <- unique(val)

  #--- anything else is an error
  } else {
    rlang::abort(c(
      x = sprintf(
        "Filter must resolve to a character, logical, or integer vector."),
      i = sprintf("Got an object of class '%s'.", class(val)[1])
    ))
  }

  return(idx)

}

#-------------------------------------------------------------------------------
# Map a subset of @edges rows to the corresponding igraph edge IDs.
# For undirected graphs each @edges row corresponds to a single igraph edge,
# looked up via igraph::get.edge.ids(..., directed = FALSE).
# For simplified directed graphs, a row whose arrowType is +/-3 represents a
# MUTUAL pair -- two distinct igraph edges (A->B and B->A). Both IDs are
# collected so that deleting the logical row removes both underlying edges.
# Returns a unique integer vector of igraph edge IDs, excluding any 0 values
# returned by get.edge.ids() when an edge is not found.
.gs_get_edge_ids <- function(g, edges_df) {

  if (nrow(edges_df) == 0L) {
    return(integer(0L))
  }

  is_dir <- igraph::is_directed(g)

  ids <- lapply(seq_len(nrow(edges_df)), function(i) {

    n1 <- edges_df$name1[i]
    n2 <- edges_df$name2[i]

    if (is_dir) {
      fwd <- igraph::get.edge.ids(g, vp = c(n1, n2), error = FALSE)
      # arrowType +/-3 signals a mutual pair: collect the reverse edge too
      if (isTRUE(abs(edges_df$arrowType[i]) == 3L)) {
        bwd <- igraph::get.edge.ids(g, vp = c(n2, n1), error = FALSE)
        c(fwd, bwd)
      } else {
        fwd
      }
    } else {
      igraph::get.edge.ids(
        g, vp = c(n1, n2), directed = FALSE, error = FALSE)
    }

  })

  ids <- unique(unlist(ids, use.names = FALSE))
  ids[ids > 0L]

}

#-------------------------------------------------------------------------------
.trim_graph_space <- function(gs, nodes) {
  
  # Remove edges whose endpoints are no longer in the node set
  edges <- gs@edges
  idx <- (edges$name1 %in% nodes$name) &
    (edges$name2 %in% nodes$name)
  edges <- edges[idx, ]
  
  # Re-map vertex index
  nodes$vertex <- seq_len(nrow(nodes))
  edges$vertex1 <- match(edges$name1, nodes$name)
  edges$vertex2 <- match(edges$name2, nodes$name)
  rownames(edges) <- NULL
  gs@edges <- edges
  gs@nodes <- nodes
  
  # Update graph vertices
  idx <- V(gs@graph)$name %in% nodes$name
  gs@graph <- igraph::delete_vertices(gs@graph, which(!idx))
  
  # Update coords
  if (nrow(gs@coords) > 0) {
    keep <- nodes$name[nodes$name %in% rownames(gs@coords)]
    gs@coords <- gs@coords[keep, , drop = FALSE]
  }
  
  # Update fdata
  if (nrow(gs@fdata) > 0) {
    keep <- nodes$name[nodes$name %in% rownames(gs@fdata)]
    gs@fdata <- gs@fdata[keep, , drop = FALSE]
  }
  
  return(gs)
  
}
