
#-------------------------------------------------------------------------------
#' @title Accessors and attribute utilities for GraphSpace objects
#' 
#' @description Access and modify individual components of a
#' \linkS4class{GraphSpace} object. Selected \pkg{igraph} methods are
#' applied to the internal graph representation and propagated to
#' downstream node and edge components.
#' 
#' @param x A \linkS4class{GraphSpace} class object
#' @param name Name of the attribute.
#' @param value Replacement value for the selected slot or attribute.
#' @param ... Additional arguments passed to extraction methods. 
#' @details
#' For \code{gs_nodes()}, the optional \code{vars} argument specifies
#' node-associated features retrieved from the \code{fdata}
#' container. See also \code{\link{gs_fetch_features}}.
#' @return Updated \linkS4class{GraphSpace} object.
#' @seealso \code{\link[igraph]{vertex_attr}}, \code{\link[igraph]{edge_attr}}, 
#' \code{\link{gs_fetch_features}}
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
#' # Vertex names
#' names(gs)
#' 
#' # Vertex attribute names
#' gs_names(gs)
#' 
#' # Get a data frame with nodes
#' gs_nodes(gs)
#' 
#' # Get a data frame with edges
#' gs_edges(gs)
#' 
#' # Get vertex count
#' gs_vcount(gs)
#' 
#' # Get edge count
#' gs_ecount(gs)
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
#' # Access a specific edge attribute
#' gs_edge_attr(gs, "edgeColor")
#' 
#' # Replace an entire edge attribute
#' gs_edge_attr(gs, "edgeLineWidth") <- 1
#' 
#' # Add an image and rescale graph coordinates to image space
#' # Images may be provided as a raster or numeric matrix
#' gs_image(gs) <- as_colorraster(volcano)
#' gs <- normalizeGraphSpace(gs, image.space = FALSE)
#' 
#' # apply a scaling factor to node coordinates
#' gs_scale_factor(gs) <- 0.1
#' # undo scaling 
#' gs_scale_factor(gs) <- 1
#' 
#' # add an 'sfc' geometry column (requires the optional 'sf' package)
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   pts <- replicate(gs_vcount(gs), sf::st_point(runif(2)), simplify = FALSE)
#'   gs_geometry(gs) <- sf::st_sfc(pts)
#' }
#' 
#' @name GraphSpace-accessors
#' @aliases names
#' @aliases gs_names
#' @aliases gs_nodes
#' @aliases gs_edges
#' @aliases gs_graph
#' @aliases gs_image
#' @aliases gs_image<-
#' @aliases gs_fdata
#' @aliases gs_fdata<-
#' @aliases gs_features
#' @aliases gs_nfeatures
#' @aliases gs_vcount
#' @aliases gs_ecount
#' @aliases gs_vertex_attr
#' @aliases gs_vertex_attr<-
#' @aliases gs_edge_attr
#' @aliases gs_edge_attr<-
#' @aliases gs_delete_v_attr
#' @aliases gs_delete_e_attr
#' @aliases gs_scale_factor
#' @aliases gs_scale_factor<-
#' @aliases gs_geometry
#' @aliases gs_geometry<-
#' @aliases gs_image_maxpixels
#' @aliases gs_image_maxpixels<-
NULL

################################################################################
### main accessors
################################################################################

#' @rdname GraphSpace-accessors
#' @aliases names,GraphSpace-method
#' @export
setMethod("names", "GraphSpace", function(x) {
  x@nodes$name
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_names", "GraphSpace", function(x) {
  colnames(x@nodes)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_nodes", "GraphSpace", function(x, ...) {
  
  args <- list(...)
  
  vars <- args$vars %||% FALSE
  
  render <- args$render %||% FALSE
  
  nodes <- if (isTRUE(render)) .gs_nodes(x) else x@nodes
  
  if (.all_characterValues(vars)) {
    
    signal_df <- gs_fetch_features(x, vars = vars, as_df = TRUE)
    
    if (!is.null(signal_df)) {
      
      signal_vars <- setdiff(colnames(signal_df), colnames(nodes) )
      
      if (length(signal_vars) > 0) {
        signal_df <- signal_df[ rownames(nodes), signal_vars, drop = FALSE]
        nodes[, signal_vars] <- signal_df
      }
      
    }
    
  }
  
  if (render) {
    attr(nodes, "gs_id") <- x@uuid
    attr(nodes, "gs_handler_type") <- "node"
    class(nodes) <- c("gs_nodes", class(nodes))
  }
  
  return(nodes)
  
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_edges", "GraphSpace", function(x, ...) {
  
  args <- list(...)
  
  render <- args$render %||% FALSE
  
  if(isFALSE(render)) return(x@edges)
  
  edges <- .gs_edges(x)
  attr(edges, "gs_id") <- x@uuid
  attr(edges, "gs_handler_type") <- "edge"
  class(edges) <- c("gs_edges", class(edges))
  return(edges)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_image", "GraphSpace", function(x) {
  
  .check_outdated_gs(x, c("image", "canvas"), type = "abort")
  
  # NOTE: returns the display CANVAS (materialized window via .get_canvas),
  # not @image. The `gs_image<-` setter writes @image -- getter/setter are
  # intentionally asymmetric: the "image" a caller sees is the rendered canvas
  .get_canvas(x)
  
})

#' @rdname GraphSpace-accessors
#' @export
setReplaceMethod("gs_image", "GraphSpace", function(x, value) {

  .check_outdated_gs(x, c("image", "canvas"), type = "abort")
  
  # Lazy image: a terra SpatRaster is stored as-is;
  # The display canvas is built from it during normalizeGraphSpace().
  if (is.raster(value) || inherits(value, "SpatRaster")) {
    x@image <- value
  } else if(is.matrix(value)){
    .validate_gs_args("numeric_mtx", "value", value)
    .validate_gs_args("numeric_mtx", "value", value)
    rlang::inform(
      c("i" = "Rasterizing numeric matrix.",
        "*" = "Values outside [0,1] are rescaled before conversion.")
    )
    rng <- range(value, na.rm = TRUE)
    if (diff(rng) == 0) {
      if (rng[1] < 0 || rng[1] > 1) {
        value[] <- 0
      }
    } else if (rng[1] < 0 || rng[2] > 1) {
      value <- (value - rng[1]) / diff(rng)
    }
    x@image <- as.raster(value)
  } else {
    rlang::abort(
      "`value` must be a 'SpatRaster', 'raster', or numeric matrix."
    )
  }
  
  return(x)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_image_maxpixels", "GraphSpace", function(x) {
  x@pars$image.maxpixels %||% 4e6
})

#' @rdname GraphSpace-accessors
#' @export
setReplaceMethod("gs_image_maxpixels", "GraphSpace", function(x, value) {
  .validate_gs_args("singleNumber", "value", value)
  x@pars$image.maxpixels <- value
  return(x)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_graph", "GraphSpace", function(x) {
  g <- x@graph
  attr(g, "gs_handler_type") <- "graph"
  class(g) <- c("gs_graph", class(g))
  return(g)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_fdata", "GraphSpace", function(x) {
  
  .check_outdated_gs(x, "fdata", type = "abort")
  
  x@fdata
  
})

#' @rdname GraphSpace-accessors
#' @export
setReplaceMethod("gs_fdata", "GraphSpace", function(x, value) {
  
  .check_outdated_gs(x, "fdata", type = "abort")
  
  x <- gs_add_features(x, value)
  
  return(x)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_nfeatures", "GraphSpace", function(x) {
  
  .check_outdated_gs(x, "fdata", type = "abort")
  
  ncol(x@fdata)
  
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_features", "GraphSpace", function(x) {
  
  .check_outdated_gs(x, "fdata", type = "abort")
  
  colnames(x@fdata)
  
})

################################################################################
### igraph accessors
################################################################################

#' @rdname GraphSpace-accessors
#' @method as.igraph GraphSpace
#' @export
as.igraph.GraphSpace <- function(x, ...) {
  return(x@graph)
}

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_vcount", "GraphSpace", function(x) {
  igraph::vcount(x@graph)
})

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_ecount", "GraphSpace", function(x) {
    igraph::ecount(x@graph)
})

#' @rdname GraphSpace-accessors
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

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_vertex_attr<-", "GraphSpace", function(x, name, ..., value) {
  
  .validate_gs_args("singleString", "name", name)
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

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_delete_v_attr", "GraphSpace", function(x, name) {
  
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
})


#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_delete_e_attr", "GraphSpace", function(x, name) {
  
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
})

#' @rdname GraphSpace-accessors
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

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_edge_attr<-", "GraphSpace", function(x, name, ..., value) {
  
  # Check protected attributes
  .validate_gs_args("singleString", "name", name)
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

.updateEdgeSpace <- function(x, g){
  x@graph <- .validate_igraph(g, simplify = .is_simplified(x))
  x@edges <- .get_edges(x@graph, simplify = .is_simplified(x))
  return(x)
}

.updateNodeSpace <- function(x, g) {
  
  x@graph <- .validate_igraph(g, simplify = .is_simplified(x))
  
  nodes <- .get_nodes(x@graph)
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
### Layout accessors (todo)
################################################################################

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_scale_factor", "GraphSpace", function(x) {
  x@pars$scale.factor %||% 1
})

#' @rdname GraphSpace-accessors
#' @export
setReplaceMethod("gs_scale_factor", "GraphSpace", function(x, value) {
  .validate_gs_args("singleNumber", "value", value)
  x@pars$scale.factor <- value
  x@coords$x <- igraph::V(x@graph)$x * value
  x@coords$y <- igraph::V(x@graph)$y * value
  x <- .denormalize_graph_space(x, verbose = .is_normalized(x))
  x
})

################################################################################
### Geometry accessors
################################################################################

#' @rdname GraphSpace-accessors
#' @export
setMethod("gs_geometry", "GraphSpace", function(x, name = "geometry") {
  x@nodes[[name]]
})

#' @rdname GraphSpace-accessors
#' @export
setReplaceMethod("gs_geometry", "GraphSpace", function(x, 
  name = "geometry", value) {
  .gs_require_sf()
  .add_node_geometry(x, name, value)
})

################################################################################
### .DollarNames accessors
################################################################################

#' @rdname GraphSpace-accessors
#' @aliases $,GraphSpace-method
#' @export
setMethod("$", "GraphSpace", function(x, name) {
  
  nodes <- x@nodes
  
  if (!(name %in% names(nodes))) {
    return(NULL)
  }
  
  nodes[[name]]
})

#' @rdname GraphSpace-accessors
#' @aliases $<-,GraphSpace-method [[<-,GraphSpace-method
#' @export
setReplaceMethod("$", "GraphSpace", function(x, name, value) {
  
  if (name %in% .gs_protected_node_cols()) {
    rlang::abort(c(
      x = sprintf("'%s' is a read-only node attribute.", name),
      i = "It is maintained internally and cannot be set directly."
    ))
  }
  if (name %in% igraph::vertex_attr_names(x@graph)) {
    # existing graph attribute: keep graph and @nodes in sync
    gs_vertex_attr(x, name) <- value
  } else if (.is_valid_geometry(value)) {
    x <- .add_node_geometry(x, name, value)
  } else {
    # payloads: new or table-only attribute, write to @nodes only
    x@nodes[[name]] <- value
  }
  x
})

################################################################################
### Internal for GraphSpace objects
################################################################################
#' Internal methods for GraphSpace
#' 
#' @description 
#' Exported solely to enable RStudio auto-completion 
#' and should not be called directly by the user.
#' 
#' @param x,pattern Internal arguments.
#' @keywords internal
#' @name GraphSpace-internal
NULL

#' @rdname GraphSpace-internal
#' @importFrom utils .DollarNames
#' @method .DollarNames GraphSpace
#' @keywords internal
#' @export
.DollarNames.GraphSpace <- function(x, pattern = "") {
  grep(pattern, names(x@nodes), value = TRUE)
}


################################################################################
### Internal for GraphSpace objects
################################################################################
#' Apply an igraph function to the graph inside a GraphSpace
#'
#' @description
#' `gs_compute()` runs any \pkg{igraph} function on the graph carried by a
#' `GraphSpace`, without needing a dedicated `gs_*` wrapper for each one. It
#' extracts the underlying igraph via [as.igraph()], applies `.f`, and returns
#' the result unchanged. This is the read-only lane onto the whole igraph
#' ecosystem: measures such as `degree()`, `betweenness()`, `coreness()`,
#' community detection, and distances all work through this one entry point.
#'
#' It is deliberately *not* a graph-modification path. If `.f` returns a graph
#' (e.g. `simplify()`, `induced_subgraph()`), `gs_compute()` errors, because
#' reintegrating a modified graph must go through the graph-modification verb
#' so that node, edge, and coordinate data stay consistent.
#'
#' @param gs A `GraphSpace` object.
#' @param .f An \pkg{igraph} function, or the name of one as a string.
#' @param ... Further arguments passed on to `.f`.
#'
#' @return Whatever `.f` returns (typically a named vector, matrix, or
#' summary), aligned to the graph's vertex order.
#'
#' @examples
#' \dontrun{
#' gs_compute(gs, degree)
#' gs_compute(gs, "betweenness", directed = FALSE)
#' gs_compute(gs, cluster_louvain)
#'
#' ## fold a per-vertex result back as a node attribute:
#' gs$degree <- gs_compute(gs, degree)
#' }
#'
#' @name gs_compute
#' @importFrom igraph as.igraph is_igraph
#' @export
gs_compute <- function(gs, .f, ...) {
  
  if (!methods::is(gs, "GraphSpace")) {
    rlang::abort(c(
      "`gs` must be a <GraphSpace> object.",
      x = sprintf("Got an object of class <%s>.", class(gs)[1])
    ))
  }
  
  f   <- .gs_resolve_fun(.f)
  g   <- igraph::as.igraph(gs)        # the read seam: the bare @graph
  out <- f(g, ...)
  
  if (igraph::is_igraph(out)) {
    rlang::abort(c(
      "`.f` returned an <igraph>, which `gs_compute()` does not reintegrate.",
      i = "Use the graph-modification verb so node, edge and coordinate data stay consistent.",
      i = "`gs_compute()` is for read-only measures returning vectors, matrices or summaries."
    ))
  }
  
  out
}

#' @keywords internal
.gs_resolve_fun <- function(.f) {
  if (is.function(.f)) return(.f)
  if (is.character(.f) && length(.f) == 1L) {
    # prefer igraph's own function, so `gs_compute(gs, "degree")` works
    # even when igraph is imported but not attached.
    if (exists(.f, envir = asNamespace("igraph"), inherits = FALSE)) {
      return(get(.f, envir = asNamespace("igraph")))
    }
    return(match.fun(.f))
  }
  rlang::abort(c(
    "`.f` must be a function or the name of a function.",
    x = sprintf("Got an object of class <%s>.", class(.f)[1])
  ))
}
