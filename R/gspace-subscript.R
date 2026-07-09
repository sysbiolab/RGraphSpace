
################################################################################
### Subscript methods for GraphSpace objects
################################################################################

#-------------------------------------------------------------------------------
#' @title Subscript operators for GraphSpace objects
#' 
#' @description
#' \code{[} subsets a \code{\link{GraphSpace}} object along two independent
#' dimensions: the first index (\code{i}) selects nodes; the second (\code{j})
#' selects edges.
#' 
#' \code{[[} retrieves a single named slot from a
#' \code{\link{GraphSpace}} object.
#' 
#' @param x A \code{\link{GraphSpace}} object.
#' @param i A node selection. Accepted forms:
#' \itemize{
#'   \item A \strong{character} vector of node names.
#'   \item An \strong{integer} vector of positional indices into \code{@nodes}.
#'   \item A \strong{logical} vector whose length matches the number of nodes.
#' }
#' If omitted, all nodes are retained.
#' 
#' @param j An edge selection. Accepted forms:
#' \itemize{
#'   \item An \strong{integer} vector of positional indices into \code{@edges}.
#'   \item A \strong{logical} vector whose length matches the number of edges.
#' }
#' Because \code{[} evaluates its arguments in the calling environment before
#' dispatch, unquoted column names such as \code{name1 == "n1"} cannot be
#' used directly. Pre-evaluate the expression against the edge table first
#' (e.g. \code{gs_edges(gs)$name1 == "n1"}), or use
#' \code{\link{gs_subset_edges}} which supports unquoted predicates via data
#' masking. If omitted, all edges are retained (subject to node-filter
#' cascade).
#' 
#' @param ... Currently unused.
#' @param drop Ignored; accepted for S4 method compatibility only.
#' 
#' @details
#' \strong{Mental model:} unlike a data frame, where \code{[i, j]} means rows
#' and columns of the same table, for \code{GraphSpace} the two indices
#' address the two primary components of the graph: nodes (\code{i}) and
#' edges (\code{j}). Neither index subsets columns — they select graph
#' entities.
#'
#' \strong{Synchronization rules:}
#' \itemize{
#'   \item \code{x[i, ]} — node-induced subgraph. After selecting nodes,
#'   edges are automatically pruned to those whose both endpoints survived.
#'   Normalized coordinates are preserved.
#'
#'   \item \code{x[, j]} — edge selection. The node set is not modified;
#'   no node pruning occurs.
#'
#'   \item \code{x[i, j]} — combined selection. Node filtering is applied
#'   first. Edge index \code{j} is resolved against the \strong{original}
#'   edge table; an edge survives only if it appears in \code{j} \emph{and}
#'   both its endpoints survived node filtering (silent intersection).
#' }
#'
#' \strong{Note for \code{[[}:} the slot accessor is read-only. Use the
#' dedicated replacement methods (\code{\link{gs_image<-}},
#' \code{\link{gs_fdata<-}}, \code{\link{gs_vertex_attr<-}}, etc.) to
#' modify slot contents.
#'
#' @return
#' \code{[} returns a \code{\link{GraphSpace}} object.
#'
#' \code{[[} returns the content of the named slot.
#'
#' @seealso
#' \code{\link{gs_subset_nodes}}, \code{\link{gs_subset_edges}},
#' \code{\link{getGraphSpace}}, \code{\link{cropGraphSpace}}
#'
#' @examples
#' library(RGraphSpace)
#' library(igraph)
#'
#' g <- make_star(10, mode = "out")
#' V(g)$nodeSize <- runif(vcount(g), 1, 10)
#' E(g)$weight   <- runif(ecount(g), 0, 1)
#' gs <- GraphSpace(g)
#' gs <- normalizeGraphSpace(gs)
#'
#' #--- [ examples ---
#'
#' # Node-induced subgraph: keep named nodes, prune dangling edges
#' gs[c("n1", "n2", "n3"), ]
#'
#' # Node-induced subgraph by integer position
#' gs[1:4, ]
#'
#' # Node-induced subgraph by pre-evaluated logical mask
#' gs[gs$nodeSize > 5, ]
#'
#' # Edge selection only: keep all nodes
#' gs[, 1:3]
#' gs[, gs_edges(gs)$weight > 0.5]
#' 
#' # Edge selection by endpoint: 'name1' and 'name2' must be pre-evaluated
#' # when using [, because [ evaluates j in the calling environment.
#' # Use gs_subset_edges() for unquoted predicate expressions instead.
#' gs[, gs_edges(gs)$name1 == "n1"]
#' gs[, gs_edges(gs)$name1 == "n1" & gs_edges(gs)$name2 == "n2"]
#' gs[, quote(name1 == "n1" & name2 == "n2")]
#'
#' # Combined: node filter first, then edge intersection
#' gs[c("n1", "n2", "n3"), gs_edges(gs)$weight > 0.5]
#' gs[c("n1", "n2", "n3"), gs_edges(gs)$name1 == "n1"]
#'
#' #--- [[ examples ---
#'
#' gs[["nodes"]]   # same as getGraphSpace(gs, "nodes")
#' gs[["edges"]]   # same as getGraphSpace(gs, "edges")
#' gs[["graph"]]   # same as getGraphSpace(gs, "graph")
#' gs[["fdata"]]   # same as getGraphSpace(gs, "fdata")
#'
#' @name GraphSpace-subscript
#' @aliases [,GraphSpace-method [[,GraphSpace-method
NULL

#-------------------------------------------------------------------------------
#' @rdname GraphSpace-subscript
#' @export
setMethod("[", "GraphSpace", function(x, i, j, ..., drop = TRUE) {

  i_missing <- missing(i)
  j_missing <- missing(j)

  if (i_missing && j_missing) {
    return(x)
  }

  nodes <- x@nodes
  edges <- x@edges

  # ---- Combined [i, j] ------------------------------------------------------
  # j must be resolved against the ORIGINAL @edges before node filtering,
  # because .trim_graph_space() may remove rows and shift positions.
  # After node filtering, surviving edges are intersected with the j set
  # by matching (name1, name2) pairs, which serve as the stable edge
  # identifier throughout the GraphSpace API.
  if (!i_missing && !j_missing) {

    j_idx <- .resolve_gs_index_direct(j, data = edges, what = "edge")

    # Capture the j-selected pairs from the original table
    j_keys <- .gs_edge_keys(edges$name1[j_idx], edges$name2[j_idx])

    x <- gs_subset_nodes(x, i)

    if (nrow(x@edges) > 0L) {
      cur_keys  <- .gs_edge_keys(x@edges$name1, x@edges$name2)
      keep_mask <- cur_keys %in% j_keys
      if (!all(keep_mask)) {
        x <- gs_subset_edges(x, keep_mask)
      }
    }

  # ---- Node-only [i, ] ------------------------------------------------------
  } else if (!i_missing) {

    x <- gs_subset_nodes(x, i)

  # ---- Edge-only [, j] ------------------------------------------------------
  } else {

    x <- gs_subset_edges(x, j)

  }

  return(x)

})

#-------------------------------------------------------------------------------
#' @rdname GraphSpace-subscript
#' @export
setMethod("[[", "GraphSpace", function(x, i, j, ...) {

  valid_slots <- c("nodes", "edges", "graph", "image", "canvas", "fdata",
    "pars", "misc")

  if (!is.character(i) || length(i) != 1L || is.na(i)) {
    rlang::abort(c(
      x = "[[' requires a single non-NA string.",
      i = paste0("Valid options: ",
        paste(paste0("'", valid_slots, "'"), collapse = ", "), ".")
    ))
  }

  if (!i %in% valid_slots) {
    rlang::abort(c(
      x = sprintf("'%s' is not a valid GraphSpace slot name.", i),
      i = paste0("Valid options: ",
        paste(paste0("'", valid_slots, "'"), collapse = ", "), ".")
    ))
  }

  getGraphSpace(x, what = i)

})

################################################################################
### Internal helpers
################################################################################

#-------------------------------------------------------------------------------
# Resolve a pre-evaluated index value (as produced by [ argument evaluation)
# against a node or edge data frame.
# Returns an integer vector of row positions to KEEP.
# Mirrors the logic of .resolve_gs_index() but accepts a plain R value
# rather than a quosure, avoiding any NSE overhead in the [ context.
#
.resolve_gs_index_direct <- function(val, data, what = "node") {

  n <- nrow(data)
  # --- language: a quoted call (e.g. quote(name1 == "n1")) -------------------
  # Re-evaluate against the data frame to obtain the actual result.
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

  if (is.character(val)) {

    if (what != "node") {
      rlang::abort(c(
        x = "Character indexing is not supported for edges.",
        i = "Use a logical or integer vector instead."
      ))
    }

    id_col <- data[["name"]]
    idx    <- which(id_col %in% val)

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

  } else if (is.logical(val)) {

    if (length(val) != n) {
      rlang::abort(sprintf(
        "Logical index length (%d) must equal the number of %ss (%d).",
        length(val), what, n
      ))
    }
    val[is.na(val)] <- FALSE
    idx <- which(val)

  } else if (is.numeric(val) || is.integer(val)) {

    val    <- as.integer(val)
    na_pos <- is.na(val)
    if (any(na_pos)) {
      rlang::warn(sprintf("%d NA index value(s) removed.", sum(na_pos)))
      val <- val[!na_pos]
    }
    if (length(val) > 0L && (any(val < 1L) || any(val > n))) {
      rlang::abort(sprintf(
        "Integer index out of range: valid positions for %ss are [1, %d].",
        what, n
      ))
    }
    idx <- unique(val)

  } else {
    rlang::abort(c(
      x = "Index must be a character, logical, or integer vector.",
      i = sprintf("Got: '%s'.", class(val)[1])
    ))
  }

  return(idx)

}

#-------------------------------------------------------------------------------
# Produce a character key for each edge, used to match edges across two
# versions of @edges (before and after node filtering) during the combined
# [i, j] operation.
#
# Keys are length-prefix encoded: the character counts of name1 and name2 are
# prepended before concatenation, making the key unambiguous regardless of
# what characters the node names contain.
#
# Example: name1 = "A", name2 = "BC"  ->  "1.2.ABC"
#          name1 = "AB", name2 = "C"  ->  "2.1.ABC"  (distinct key) 
#
.gs_edge_keys <- function(name1, name2) {
  paste0(nchar(name1), ".", nchar(name2), ".", name1, name2)
}
