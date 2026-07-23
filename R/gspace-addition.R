
################################################################################
### Addition methods for GraphSpace objects
################################################################################

#-------------------------------------------------------------------------------
#' @title Add edges to a GraphSpace object
#'
#' @description
#' \code{gs_add_edges()} and \code{gs_add_edges<-} add one or more edges to a
#' \code{\link{GraphSpace}} object. Both endpoints of every new edge must
#' already exist in the node set. The \code{@graph}, \code{@edges}, and
#' all derived edge quantities are updated consistently; the node set and
#' the normalized coordinate state are not affected.
#'
#' \code{gs_add_edges(x, value)} is the pipe-friendly functional form and
#' returns the modified object. \code{gs_add_edges(x) <- value} is the
#' in-place replacement form and modifies \code{x} by reference in the
#' calling environment. Both forms are equivalent.
#'
#' @param x A \code{\link{GraphSpace}} object.
#' @param value A data frame with at least two columns identifying the edge
#' endpoints. Two column naming conventions are accepted:
#' \itemize{
#'   \item \code{from} / \code{to} — the tidygraph / igraph convention.
#'   \item \code{name1} / \code{name2} — the \code{@edges} slot convention,
#'     useful when constructing \code{value} directly from \code{gs_edges()}.
#' }
#' If both conventions are present, \code{from}/\code{to} takes priority.
#' Any additional columns are treated as edge attributes and passed through
#' to \code{@edges}. Standard visual attributes (\code{edgeColor},
#' \code{arrowType}, etc.) are filled from package defaults when omitted;
#' analytical attributes such as \code{weight} are stored as-is.
#' @param ... Additional arguments (currently unused; reserved for future use).
#'
#' @details
#' Adding edges does not invalidate the normalized layout. Node coordinates
#' in \code{@nodes} are left untouched and \code{normalizeGraphSpace} does
#' not need to be re-run.
#'
#' For objects built with \code{simplify = TRUE} (the default), loop edges
#' (\code{from == to}), parallel edges, and duplicate rows within
#' \code{value} are silently dropped with a warning. Admissible edges in
#' the same call are still added. To allow loops or parallel edges, rebuild
#' the object with \code{GraphSpace(g, simplify = FALSE)}.
#'
#' Because adding an edge to a group of parallel edges changes the derived
#' attributes \code{curve_weight}, \code{is_multiple}, and \code{is_loop}
#' for all members of that group, the full edge table is recomputed from
#' \code{@graph} after each assignment.
#'
#' @return A \code{\link{GraphSpace}} object with the new edges appended.
#'
#' @seealso
#' \code{\link{gs_add_nodes}}, \code{\link{gs_edge_attr}},
#' \code{\link{gs_subset_edges}}, \code{\link{gs_edges}}
#'
#' @examples
#' library(RGraphSpace)
#' library(igraph)
#'
#' g <- make_star(5, mode = "out")
#' gs <- GraphSpace(g)
#' gs <- normalizeGraphSpace(gs)
#'
#' # Functional form (pipe-friendly): returns a modified copy
#' gs <- gs_add_edges(gs, data.frame(from = "n2", to = "n3"))
#'
#' # Assignment form: modifies gs in place
#' gs_add_edges(gs) <- data.frame(from = "n2", to = "n3")
#'
#' # Add multiple edges with an analytical attribute
#' gs <- gs_add_edges(gs, data.frame(
#'   from   = c("n2", "n3"),
#'   to     = c("n4", "n5"),
#'   weight = c(0.8, 0.4)
#' ))
#'
#' # name1/name2 convention also accepted (e.g. from gs_edges() output)
#' gs <- gs_add_edges(gs, data.frame(name1 = "n2", name2 = "n3"))
#'
#' @name gs_add_edges
#' @aliases gs_add_edges<-
NULL

#-------------------------------------------------------------------------------
#' @rdname gs_add_edges
#' @export
setMethod("gs_add_edges", "GraphSpace", function(x, value, ...) {
  `gs_add_edges<-`(x, value = value)
})

#-------------------------------------------------------------------------------
#' @rdname gs_add_edges
#' @export
setReplaceMethod("gs_add_edges", "GraphSpace", function(x, value) {

  #--- validate and normalize value
  if (!is.data.frame(value)) {
    rlang::abort(c(
      x = "'value' must be a data frame.",
      i = "Required columns: 'from'/'to' or 'name1'/'name2' (vertex names)."
    ))
  }

  # Accept name1/name2 as aliases for from/to; from/to takes priority
  if (!"from" %in% colnames(value) && "name1" %in% colnames(value)) {
    value$from <- value$name1
  }
  if (!"to" %in% colnames(value) && "name2" %in% colnames(value)) {
    value$to <- value$name2
  }
  # Drop name1/name2 to avoid passing them as edge attributes
  value <- value[, setdiff(colnames(value), c("name1", "name2")), drop = FALSE]

  # Strip protected and derived edge columns that originate from @edges but
  # are not user-editable attributes: passing them to igraph::add_edges()
  # would store stale values and corrupt the reconstructed @edges.
  value <- value[, setdiff(colnames(value), .gs_protected_edge_cols()), 
    drop = FALSE]

  missing_cols <- setdiff(c("from", "to"), colnames(value))
  if (length(missing_cols) > 0L) {
    rlang::abort(c(
      x = sprintf(
        "Missing required column(s) in 'value': %s.",
        paste(paste0("'", missing_cols, "'"), collapse = ", ")
      ),
      i = "Provide 'from'/'to' or 'name1'/'name2' columns containing node names."
    ))
  }

  # Coerce from/to to character: factor inputs are common from read.csv() or
  # merge() and would cause spurious mismatches in the endpoint check.
  value$from <- as.character(value$from)
  value$to   <- as.character(value$to)

  # Empty value: return x silently — a legitimate outcome in pipelines
  if (nrow(value) == 0L) return(x)

  #--- validate endpoints
  na_endpoints <- is.na(value$from) | is.na(value$to)
  if (any(na_endpoints)) {
    rlang::abort(c(
      x = sprintf(
        "%d row(s) in 'value' contain NA in 'from' or 'to'.",
        sum(na_endpoints)
      ),
      i = "Both endpoints must be non-NA node names."
    ))
  }

  node_names  <- igraph::V(x@graph)$name
  missing_ids <- setdiff(union(value$from, value$to), node_names)

  if (length(missing_ids) > 0L) {
    rlang::abort(c(
      x = "All edge endpoints must already exist in the node set.",
      i = sprintf(
        "%d unknown name(s): %s",
        length(missing_ids),
        .gs_preview(missing_ids)
      ),
      "*" = "Modify the underlying igraph object to add nodes before connecting them."
    ))
  }

  #--- check simplification constraints
  # Returns value with inadmissible edges dropped and warnings issued.
  # If nothing survives, return x unchanged.
  value <- .check_new_edges(x, value)
  if (nrow(value) == 0L) return(x)

  #--- add edges to the igraph object
  g  <- x@graph
  vp <- as.vector(rbind(value$from, value$to))

  #--- synchronise standard edge attributes across old and new edges
  # igraph backfills NA for any attribute that exists on only one side of an
  # add_edges() call. Because the validator rejects NA in standard attributes,
  # we must ensure every standard attribute is either present on both sides or
  # absent from both before the call:
  #-- old edges have it, new edges don't -> fill default into value (new edges)
  #-- new edges have it, old edges don't -> backfill default onto existing edges
  #-- present on both / absent from both -> no action needed
  existing_eatt <- igraph::edge_attr_names(g)
  defaults      <- .get_default_eatt(igraph::is_directed(g))
  for (att in names(defaults)) {
    in_graph <- att %in% existing_eatt
    in_value <- att %in% colnames(value)
    if (in_graph && !in_value) {
      value[[att]] <- defaults[[att]]
    } else if (!in_graph && in_value) {
      igraph::edge_attr(g, att) <- defaults[[att]]
    }
  }

  extra_cols <- setdiff(colnames(value), c("from", "to"))

  if (length(extra_cols) > 0L) {
    attr_list <- as.list(value[, extra_cols, drop = FALSE])
    g <- do.call(igraph::add_edges, c(list(graph = g, edges = vp), attr_list))
  } else {
    g <- igraph::add_edges(g, vp)
  }

  #--- rebuild @edges via the authoritative update path
  # .updateEdgeSpace() re-validates the igraph object
  x <- .updateEdgeSpace(x, g)

  validObject(x)
  return(x)

})

################################################################################
### Internal helpers
################################################################################

#-------------------------------------------------------------------------------
# Check new edges against simplification constraints.
# For simplified objects, loops, parallel edges, and duplicate rows within
# value are inadmissible. Rather than aborting, inadmissible rows are dropped
# and a warning is issued. Returns the filtered value (may have 0 rows).
.check_new_edges <- function(x, value) {

  if (!.is_simplified(x)) return(value)

  g      <- x@graph
  is_dir <- igraph::is_directed(g)

  #--- loops
  is_loop <- value$from == value$to
  if (any(is_loop)) {
    rlang::warn(c(
      "!" = sprintf(
        "%d loop edge(s) ignored: simplified GraphSpace do not allow loops.",
        sum(is_loop)
      ),
      "i" = sprintf(
        "Affected loops (showing %d of %d): %s",
        min(sum(is_loop), 3L),
        sum(is_loop),
        .gs_preview(paste0(value$from[is_loop], " -> ", value$to[is_loop]))
      )
    ))
    value <- value[!is_loop, , drop = FALSE]
    rownames(value) <- NULL
  }

  if (nrow(value) == 0L) return(value)

  #--- duplicates within value
  pair_keys  <- paste(value$from, value$to, sep = "\x01")
  is_dup     <- duplicated(pair_keys)
  if (any(is_dup)) {
    rlang::warn(c(
      "!" = sprintf(
        "%d duplicate edge(s) ignored: each (from, to) pair is kept only once.",
        sum(is_dup)
      ),
      "i" = sprintf(
        "Affected pairs (showing %d of %d): %s",
        min(sum(is_dup), 3L),
        sum(is_dup),
        .gs_preview(paste0(value$from[is_dup], " -> ", value$to[is_dup]))
      )
    ))
    value <- value[!is_dup, , drop = FALSE]
    rownames(value) <- NULL
  }

  if (nrow(value) == 0L) return(value)

  #--- parallel edges (against existing graph)
  is_parallel <- vapply(seq_len(nrow(value)), function(i) {
    n1 <- value$from[i]
    n2 <- value$to[i]
    if (is_dir) {
      igraph::get.edge.ids(g, vp = c(n1, n2), error = FALSE) > 0L
    } else {
      igraph::get.edge.ids(
        g, vp = c(n1, n2), directed = FALSE, error = FALSE) > 0L
    }
  }, logical(1L))

  if (any(is_parallel)) {
    rlang::warn(c(
      "!" = sprintf(
        "%d parallel edge(s) ignored: simplified GraphSpace do not allow parallel edges.",
        sum(is_parallel)
      ),
      "i" = .gs_preview(paste0(
        value$from[is_parallel], " -> ", value$to[is_parallel])),
      "*" = "Rebuild with GraphSpace(g, simplify = FALSE) to allow parallel edges."
    ))
    value <- value[!is_parallel, , drop = FALSE]
    rownames(value) <- NULL
  }

  return(value)

}


################################################################################
### gs_add_nodes replacement method
################################################################################

#-------------------------------------------------------------------------------
#' @title Add nodes to a GraphSpace object
#'
#' @description
#' \code{gs_add_nodes()} and \code{gs_add_nodes<-} add one or more nodes to a
#' \code{\link{GraphSpace}} object. The \code{@graph}, \code{@nodes}, and
#' \code{@fdata} slots are updated consistently. Because new nodes introduce
#' coordinates into the existing layout, the normalized state is invalidated
#' and \code{\link{normalizeGraphSpace}} must be re-run afterwards.
#'
#' \code{gs_add_nodes(x, value)} is the pipe-friendly functional form and
#' returns the modified object. \code{gs_add_nodes(x) <- value} is the
#' in-place replacement form and modifies \code{x} by reference in the
#' calling environment. Both forms are equivalent.
#'
#' @param x A \code{\link{GraphSpace}} object.
#' @param value A data frame with at minimum three columns:
#' \itemize{
#'   \item \code{name} — unique node identifier (character).
#'   \item \code{x}, \code{y} — node coordinates in raw graph space.
#' }
#' Any additional columns are treated as node attributes. Standard visual
#' attributes (\code{nodeSize}, \code{nodeColor}, \code{nodeShape}, etc.)
#' are filled from package defaults when omitted. The column \code{vertex}
#' is reserved and stripped automatically if present.
#' @param ... Additional arguments (currently unused; reserved for future use).
#'
#' @details
#' Adding nodes always invalidates the normalized layout. The \code{@pars}
#' normalization flags are cleared, \code{@canvas} is reset, and
#' \code{normalizeGraphSpace} must be re-run to restore a renderable state.
#' The \code{@edges} slot is not affected. Existing node coordinates in
#' \code{@nodes} revert to raw graph-space values if the object was previously
#' normalized, since normalization is cleared before \code{@nodes} is rebuilt.
#'
#' Standard node attributes (\code{nodeSize}, \code{nodeColor}, etc.) are
#' kept consistent across old and new nodes: attributes present on existing
#' nodes but absent from \code{value} are filled from package defaults for
#' the new rows, and vice versa.
#'
#' \code{nodeLabel} defaults to the node \code{name} when not supplied,
#' consistent with the behaviour of the \code{\link{GraphSpace}} constructor.
#'
#' If \code{@fdata} is non-empty, new nodes are appended as \code{NA} rows
#' so the feature matrix remains aligned with \code{@nodes}.
#'
#' @return A \code{\link{GraphSpace}} object with the new nodes appended and
#' the normalized state cleared.
#'
#' @seealso
#' \code{\link{gs_add_edges}}, \code{\link{gs_vertex_attr}},
#' \code{\link{gs_subset_nodes}}, \code{\link{gs_nodes}}
#'
#' @examples
#' library(RGraphSpace)
#' library(igraph)
#'
#' g <- make_star(5, mode = "out")
#' gs <- GraphSpace(g)
#'
#' # Functional form (pipe-friendly): returns a modified copy
#' gs <- gs_add_nodes(gs, data.frame(name = "n6", x = 0.5, y = 0.5))
#'
#' # Assignment form: modifies gs in place
#' gs_add_nodes(gs) <- data.frame(name = "n7", x = 0.5, y = 0.5)
#'
#' # Add multiple nodes with visual attributes
#' gs <- gs_add_nodes(gs, data.frame(
#'   name      = c("n8", "n9"),
#'   x         = c(0.5, 0.8),
#'   y         = c(0.5, 0.2),
#'   nodeSize  = c(8, 5),
#'   nodeColor = c("steelblue", "tomato")
#' ))
#'
#' @name gs_add_nodes
#' @aliases gs_add_nodes<-
NULL

#-------------------------------------------------------------------------------
#' @rdname gs_add_nodes
#' @export
setMethod("gs_add_nodes", "GraphSpace", function(x, value, ...) {
  `gs_add_nodes<-`(x, value = value)
})

#-------------------------------------------------------------------------------
#' @rdname gs_add_nodes
#' @export
setReplaceMethod("gs_add_nodes", "GraphSpace", function(x, value) {
  
  #--- validate and normalize value
  if (!is.data.frame(value)) {
    rlang::abort(c(
      x = "'value' must be a data frame.",
      i = "Required columns: 'name' (node identifier), 'x' and 'y' (coordinates)."
    ))
  }
  
  # Strip vertex (auto-assigned) and away_angle (render-only derived column
  # added by gs_nodes(render = TRUE)): neither is a user-editable attribute.
  value <- value[, setdiff(colnames(value), c("vertex", "away_angle")), drop = FALSE]
  
  missing_cols <- setdiff(c("name", "x", "y"), colnames(value))
  if (length(missing_cols) > 0L) {
    rlang::abort(c(
      x = sprintf(
        "Missing required column(s) in 'value': %s.",
        paste(paste0("'", missing_cols, "'"), collapse = ", ")
      ),
      i = "Provide 'name' (character), 'x' and 'y' (numeric coordinates)."
    ))
  }
  
  # Coerce required columns to their expected types
  value$name <- as.character(value$name)
  value$x <- as.numeric(value$x)
  value$y <- as.numeric(value$y)
  
  #--- NA check on required columns
  na_rows <- is.na(value$name) | is.na(value$x) | is.na(value$y)
  if (any(na_rows)) {
    rlang::abort(c(
      x = sprintf(
        "%d row(s) in 'value' contain NA in 'name', 'x', or 'y'.",
        sum(na_rows)
      ),
      i = "All three required columns must be non-NA."
    ))
  }
  
  # Empty value: return x silently -- a legitimate outcome in pipelines
  if (nrow(value) == 0L) return(x)
  
  #--- duplicate names within value
  is_dup <- duplicated(value$name)
  if (any(is_dup)) {
    rlang::warn(c(
      "!" = sprintf(
        "%d duplicate node name(s) ignored: each name is kept only once.",
        sum(is_dup)
      ),
      "i" = sprintf(
        "Affected names (showing %d of %d): %s",
        min(sum(is_dup), 3L),
        sum(is_dup),
        .gs_preview(value$name[is_dup])
      )
    ))
    value <- value[!is_dup, , drop = FALSE]
    rownames(value) <- NULL
  }
  
  if (nrow(value) == 0L) return(x)
  
  #--- name conflicts with existing nodes
  existing_names <- igraph::V(x@graph)$name
  conflicts <- intersect(value$name, existing_names)
  if (length(conflicts) > 0L) {
    rlang::abort(c(
      x = "Node names must be unique in a GraphSpace object.",
      i = sprintf("%d name(s) already exist: %s",
        length(conflicts), .gs_preview(conflicts)
      )
    ))
  }
  
  #--- normalization warning
  # image-space always implies normalization, so a single message covers both.
  if (.is_normalized(x) || .is_image_space(x)) {
    msg <- if (.is_image_space(x)) {
      "Adding nodes to an image-space object invalidates the image normalized layout."
    } else {
      "Adding nodes invalidates the normalized layout."
    }
    rlang::warn(c(
      "!" = msg,
      "i" = "Re-run normalizeGraphSpace() after adding nodes to restore it."
    ))
  }
  
  #--- synchronize standard node attributes
  # igraph backfills NA for any attribute that exists on only one side of an
  # add_vertices() call. Ensure every standard attribute is either present on
  # both sides or absent from both before the call:
  #--old nodes have it, new nodes don't -> fill default into value
  #--new nodes have it, old nodes don't -> backfill default onto existing nodes
  #--present on both / absent from both -> no action needed
  #
  # nodeLabel is handled first: its default is the node name, not a fixed
  # value, matching the constructor behavior.
  g <- x@graph
  existing_vatt <- igraph::vertex_attr_names(g)
  defaults <- .get_default_vatt()
  
  if (!"nodeLabel" %in% colnames(value) && "nodeLabel" %in% existing_vatt) {
    value$nodeLabel <- value$name
  } else if ("nodeLabel" %in% colnames(value) && !"nodeLabel" %in% existing_vatt) {
    igraph::vertex_attr(g, "nodeLabel") <- as.character(igraph::V(g)$name)
  }
  
  for (att in setdiff(names(defaults), "nodeLabel")) {
    in_graph <- att %in% existing_vatt
    in_value <- att %in% colnames(value)
    if (in_graph && !in_value) {
      value[[att]] <- defaults[[att]]
    } else if (!in_graph && in_value) {
      igraph::vertex_attr(g, att) <- defaults[[att]]
    }
  }
  
  #--- add vertices to the igraph object
  g <- igraph::add_vertices(g, nv = nrow(value), attr = as.list(value))
  
  # Denormalize first so .updateNodeSpace() rebuilds @nodes
  # in raw coordinates rather than restoring normalized ones.
  x <- .denormalize_graph_space(x)
  x <- .updateNodeSpace(x, g)
  
  #--- extend @fdata with NA rows for new nodes --------------------------------
  # Appended at the end, matching the order in which igraph::add_vertices()
  # places new vertices and .get_nodes() reads them back.
  if (nrow(x@fdata) > 0L) {
    new_rows <- Matrix::Matrix(
      NA_real_, nrow = nrow(value), ncol = ncol(x@fdata),
      sparse = TRUE,
      dimnames = list(value$name, colnames(x@fdata))
    )
    x@fdata <- rbind(x@fdata, new_rows)
  }
  
  validObject(x)
  return(x)
  
})
