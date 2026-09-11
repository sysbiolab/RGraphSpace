
#-------------------------------------------------------------------------------
#' @title Attribute utilities for GraphSpace objects
#' 
#' @description Access and modify individual components of a
#' \linkS4class{GraphSpace} object. Selected \pkg{igraph} methods are
#' applied to the internal graph representation and propagated to
#' downstream components.
#' 
#' @param x A \linkS4class{GraphSpace} class object
#' @param name Name of the attribute.
#' @param value Replacement value for the selected slot or attribute.
#' @param ... Additional arguments passed to extraction methods. 
#' @details
#' For ...
#' @examples
#' library(RGraphSpace)
#' library(igraph)
#' 
#' # Load a demo igraph
#' data('gtoy1', package = 'RGraphSpace')
#' 
#' # Create a new GraphSpace object
#' gs <- GraphSpace(gtoy1)
#' 
#' #--- Usage of GraphSpace attribute accessors:
#' 
#' # Access all vertex attributes
#' gs_vertex_attr(gs)
#' 
#' # Access a specific vertex attribute
#' gs_vertex_attr(gs, "nodeLabel")
#' 
#' # Modify a single value within a vertex attribute
#' gs_vertex_attr(gs, "nodeSize")["n1"] <- 10
#' 
#' # Replace an entire vertex attribute
#' gs_vertex_attr(gs, "nodeSize") <- 10
#' 
#' # Add a new vertex attribute
#' gs_vertex_attr(gs, "node_var1") <- rnorm(gs_vcount(gs))
#' 
#' # Delete a vertex attribute
#' gs_vertex_attr(gs, "node_var1") <- NULL
#' 
#' # Access a specific edge attribute
#' gs_edge_attr(gs, "edgeColor")
#' 
#' # Replace an entire edge attribute
#' gs_edge_attr(gs, "edgeLineWidth") <- 1
#' 
#' # Add a new edge attribute
#' gs_edge_attr(gs, "edge_var1") <- rnorm(gs_ecount(gs))
#' 
#' # Delete an edge attribute
#' gs_edge_attr(gs, "edge_var1") <- NULL
#' 
#' @name GraphSpace-attributes
#' @aliases gs_vertex_attr
#' @aliases gs_vertex_attr<-
#' @aliases gs_edge_attr
#' @aliases gs_edge_attr<-
NULL

#' @rdname GraphSpace-attributes
#' @export
setMethod("gs_vertex_attr", "GraphSpace", function(x, name, ..., value) {
  if(missing(value)){
    g <- x@graph
    if(missing(name)){
      att <- igraph::vertex_attr(graph = g, ...=...)
      return(att)
    } else {
      .validate_gs_args("singleString", "name", name)
      if(name %in% igraph::vertex_attr_names(g)){
        att <- igraph::vertex_attr(graph = g, name = name, ...=...)
        if(name!="name") names(att) <- V(g)$name
      } else {
        att <- NULL
      }
    }
    return(att)
  } else {
    gs_vertex_attr(x, name, ...) <- value
    return(x)
  }

})

#' @rdname GraphSpace-attributes
#' @export
setMethod("gs_vertex_attr<-", "GraphSpace", function(x, name, ..., value) {
  
  .validate_gs_args("singleString", "name", name)
  
  # Assigning NULL deletes the attribute
  if (is.null(value)) {
    x <- .gs_delete_v_attr(x, name)
    return(x)
  }
  
  # Check protected attributes
  if (name %in% .gs_protected_node_cols()) {
    rlang::abort(c(
      x = sprintf("'%s' is a read-only node attribute.", name),
      i = "It is maintained internally and cannot be set directly.",
      "*" = "To change the graph structure, recreate the underlying igraph object."
    ))
  }
  
  g <- x@graph
  if(length(value)==1){
    value <- if(.is_replicable(value)) value else list(value)
  }
  igraph::vertex_attr(graph = g, name = name, ...=...) <- value
  x <- .updateNodeSpace(x, g)
  
  return(x)
  
})

# Used to handle possible function replication
.is_replicable <- function(x) {
  tryCatch({
    rep(x, 2)
    TRUE
  }, error = function(e) FALSE)
}

#' @rdname GraphSpace-attributes
#' @export
setMethod("gs_edge_attr", "GraphSpace", function(x, name, ..., value) {
  if (missing(value)) {
    g <- x@graph
    if(missing(name)){
      att <- igraph::edge_attr(graph = g, ...=...)
    } else {
      .validate_gs_args("singleString", "name", name)
      att <- igraph::edge_attr(graph = g, name = name, ...=...)
    }
    return(att)
  } else {
    .validate_gs_args("singleString", "name", name)
    gs_edge_attr(x, name, ...) <- value
    return(x)
  }
})

#' @rdname GraphSpace-attributes
#' @export
setMethod("gs_edge_attr<-", "GraphSpace", function(x, name, ..., value) {
  
  .validate_gs_args("singleString", "name", name)
  
  # Assigning NULL deletes the attribute
  if (is.null(value)) {
    x <- .gs_delete_e_attr(x, name)
    return(x)
  }
  
  # Check protected attributes
  if (name %in% .gs_protected_edge_cols()) {
    rlang::abort(c(
      x = sprintf("'%s' is a read-only edge attribute.", name),
      i = "It is maintained internally and cannot be set directly.",
      "*" = "To change the graph structure, recreate the underlying igraph object."
    ))
  }
  
  g <- x@graph
  if(length(value)==1){
    value <- if(.is_replicable(value)) value else list(value)
  }
  igraph::edge_attr(graph = g, name = name, ...=...) <- value
  x <- .updateEdgeSpace(x, g)
  
  return(x)
  
})

#' @keywords internal
.gs_delete_v_attr <-  function(x, name) {
  
  .validate_gs_args("singleString", "name", name)
  
  if (name %in% .gs_protected_node_cols(ext=TRUE)) {
    rlang::abort(c(
      x = sprintf("'%s' is a protected node attribute.", name),
      i = "It is maintained internally and cannot be deleted."
    ))
  }
  
  g <- x@graph
  vnames <- igraph::vertex_attr_names(g)
  cnames <- c(colnames(x@nodes), colnames(x@coords))
  if(!name %in% c(vnames, cnames)){
    rlang::warn(c(x = sprintf("'%s' attribute not found.", name)))
    return(x)
  }
  
  if(name %in% vnames){
    g <- igraph::delete_vertex_attr(graph = g, name = name)
  }
  x@nodes <- x@nodes[ , colnames(x@nodes)!=name, drop = FALSE]
  x@coords <- x@coords[ , colnames(x@coords)!=name, drop = FALSE]
  x <- .updateNodeSpace(x, g)
  
  return(x)
}

#' @keywords internal
.gs_delete_e_attr <- function(x, name) {
  
  .validate_gs_args("singleString", "name", name)
  
  if (name %in% .gs_protected_edge_cols(ext=TRUE)) {
    rlang::abort(c(
      x = sprintf("'%s' is a protected edge attribute.", name),
      i = "It is maintained internally and cannot be deleted."
    ))
  }
  
  g <- x@graph
  enames <- igraph::edge_attr_names(g)
  cnames <- colnames(x@edges)
  if(!name %in% c(enames, cnames)){
    rlang::warn(c(x = sprintf("'%s' attribute not found.", name)))
    return(x)
  }
  if(name %in% enames){
    g <- igraph::delete_edge_attr(graph = g, name = name)
  }
  x@edges <- x@edges[ , colnames(x@edges)!=name, drop = FALSE]
  x <- .updateEdgeSpace(x, g)
  return(x)
}

.updateEdgeSpace <- function(x, g){
  x@graph <- .validate_igraph(g, simplify = .is_simplified(x))
  x@edges <- .build_edges(x@graph, simplify = .is_simplified(x))
  return(x)
}

.updateNodeSpace <- function(x, g) {
  
  x@graph <- .validate_igraph(g, simplify = .is_simplified(x))
  
  nodes <- .build_nodes(x@graph)
  keep <- setdiff(colnames(x@nodes), colnames(nodes))
  for (col in keep) nodes[[col]] <- x@nodes[[col]][match(nodes$name,
    x@nodes$name)]
  
  coords <- nodes[ , c("x", "y")]
  keep <- setdiff(colnames(x@coords), colnames(coords))
  for (col in keep) coords[[col]] <- x@coords[[col]][match(rownames(coords), 
    rownames(x@coords))]
  
  if (.is_normalized(x)) {
    nodes[x@nodes$name, c("x","y")] <- x@nodes[, c("x","y")]
  }
  
  x@nodes <- nodes
  x@coords <- coords
  
  return(x)
}

################################################################################
### Deprecated helpers
################################################################################
#' @title Deprecated attribute-deletion helpers
#' @description
#' Superseded by assigning \code{NULL} through \code{\link{gs_vertex_attr}} 
#' and \code{\link{gs_edge_attr}} functions, for example:
#' \preformatted{
#' gs_vertex_attr(gs, "a_node_var") <- NULL
#' gs_edge_attr(gs, "an_edge_var") <- NULL
#' }
#' @details 
#' These functions are now defunct and always error.
#' @keywords internal
#' @name deprecated-attr
#' @aliases gs_delete_v_attr
#' @aliases gs_delete_e_attr
NULL

#' @rdname deprecated-attr
#' @export
gs_delete_v_attr <- function(...) {
  lifecycle::deprecate_stop("1.5.4", 
    "gs_delete_v_attr()", 
    "`gs_vertex_attr<-`()")
}

#' @rdname deprecated-attr
#' @export
gs_delete_e_attr <- function(...) {
  lifecycle::deprecate_stop("1.5.4", 
    "gs_delete_e_attr()", 
    "`gs_edge_attr<-`()")
}
