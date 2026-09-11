
#-------------------------------------------------------------------------------
#' @title Accessors for GraphSpace objects
#' 
#' @description Access and modify individual components of a
#' \linkS4class{GraphSpace} object.
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
#' @seealso \code{\link{gs_fetch_features}}
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
#' #--- Usage of GraphSpace accessors:
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
#' # Get an igraph object
#' gs_graph(gs)
#' 
#' # Get a data frame with nodes
#' gs_nodes(gs)
#' 
#' # Vertex count
#' gs_vcount(gs)
#' 
#' # Edge count
#' gs_ecount(gs)
#' 
#' # Images may be provided as raster or numeric matrices;
#' # 'SpatRaster' objects are supported when the optional 
#' # 'terra' package is available
#' gs_image(gs) <- as_colorraster(volcano)
#' 
#' # Apply a scaling factor to node coordinates
#' gs_scale_factor(gs) <- 0.1
#' 
#' # Undo scaling 
#' gs_scale_factor(gs) <- 1
#' 
#' # Set a pixel budget for image operations
#' gs_image_maxpixels(gs) <- 4e+06
#' 
#' # Normalize image and node coordinates to graph space
#' gs <- normalizeGraphSpace(gs, image.space = FALSE)
#' 
#' # Add a sparse Matrix aligned to nodes
#' library(Matrix)
#' mtx <- Matrix(0, gs_vcount(gs), 2)
#' rownames(mtx) <- names(gs)
#' colnames(mtx) <- c("feature1","feature2")
#' gs_fdata(gs) <- mtx
#' 
#' # Feature names
#' gs_features(gs)
#' 
#' # Feature count
#' gs_nfeatures(gs)
#' 
#' # Add an 'sfc' geometry column (requires the optional 'sf' package)
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   gs_geometry(gs) <- sfshape_ngons(n = gs_vcount(gs))
#' }
#' 
#' @name GraphSpace-accessors
#' @aliases names
#' @aliases gs_names
#' @aliases gs_nodes
#' @aliases gs_edges
#' @aliases gs_graph
#' @aliases gs_vcount
#' @aliases gs_ecount
#' @aliases gs_image
#' @aliases gs_image<-
#' @aliases gs_fdata
#' @aliases gs_fdata<-
#' @aliases gs_features
#' @aliases gs_nfeatures
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
    .gs_require_sf()
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
#' Apply igraph functions to the graph inside a GraphSpace
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
#' (e.g. `simplify()`, `induced_subgraph()`), this cannot be reintegrated as 
#' a modified graph must go through the graph-modification checks.
#'
#' @param gs A `GraphSpace` object.
#' @param .f An \pkg{igraph} function, or the name of one as a string.
#' @param ... Further arguments passed on to `.f`.
#'
#' @return Whatever `.f` returns (typically a named vector, matrix, or
#' summary), aligned to the graph's vertex order.
#'
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
#' # Apply igraph functions
#' gs_compute(gs, igraph::degree)
#' gs_compute(gs, "betweenness", directed = FALSE)
#'
#' # Fold a per-vertex result back as a node attribute
#' gs$degree <- gs_compute(gs, igraph::degree)
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
  g   <- igraph::as.igraph(gs)  # the read seam: the bare @graph
  out <- f(g, ...)
  
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

