#' @importFrom methods setOldClass setClass
#' @importFrom grDevices as.raster
#' @importFrom igraph make_empty_graph
#' @importFrom tidygraph tbl_graph
setOldClass("raster")
setOldClass("igraph")
setOldClass("tbl_graph")
setOldClass("gs_graph")

#-------------------------------------------------------------------------------
#' @title GraphSpace: An S4 class for igraph objects
#'
#' @slot nodes A data frame containing node coordinates, attributes, and metadata.
#' @slot edges  A data frame containing edge relationships and attributes.
#' @slot graph An \code{\link[igraph]{igraph}} object representing the graph 
#' structure.
#' @slot image A \code{raster} object (see \code{\link[grDevices]{as.raster}})
#' holding the original background image as supplied by the user. Never
#' modified after construction; always serves as the stable source for
#' \code{normalizeGraphSpace()}.
#' @slot canvas A \code{raster} object holding the processed,
#' render-ready image produced by \code{normalizeGraphSpace()}. Receives all
#' centering, flipping, and margin adjustments. When this slot contains only
#' the empty sentinel, downstream accessors fall back to \code{@image}
#' automatically; see \link{gs_image}.
#' @slot fdata A \code{\link[Matrix]{Matrix}} object storing high-dimensional 
#' feature data associated with graph nodes.
#' @slot pars A list with parameters.
#' @slot misc A list with intermediate objects for downstream methods.
#' @slot uuid A Universally Unique Identifier (UUID) for the object instance.
#' 
#' @method plotGraphSpace \link{plotGraphSpace}
#' @method getGraphSpace \link{getGraphSpace}
#' @aliases GraphSpace-class
#' @return An S4 class object.
#' @section Constructor:
#' see \code{\link{GraphSpace}} constructor.
#' @import igraph
#' @exportClass GraphSpace
#'
## Class GraphSpace
setClass("GraphSpace",
  slots = c(
    nodes = "data.frame",
    edges = "data.frame",
    graph = "igraph",
    image = "raster",
    canvas = "raster",
    fdata = "Matrix",
    pars = "list",
    misc = "list",
    uuid = "character"
  ),
  prototype = list(
    nodes = data.frame(),
    edges = data.frame(),
    graph = igraph::make_empty_graph(),
    image = as.raster(matrix()),
    canvas = as.raster(matrix()),
    fdata = Matrix::Matrix(nrow = 0, ncol = 0),
    pars = list(),
    misc = list(),
    uuid = character()
  )
)

setValidity("GraphSpace", function(object) {
  
  errors <- character()
  
  if (!is.data.frame(object@nodes)) {
    errors <- c(errors, "'@nodes' slot must be a data.frame.")
  }
  
  if (nrow(object@nodes) > 0 && is.null(rownames(object@nodes))) {
    errors <- c(errors, "'@nodes' slot must have row names.")
  }
  
  if (!is.data.frame(object@edges)) {
    errors <- c(errors, "'@edges' slot must be a data.frame.")
  }
  
  if (!is(object@fdata, "Matrix")) {
    errors <- c(errors, "'@fdata' slot must be a Matrix object.")
  }
  
  if (nrow(object@fdata) > 0 && is.null(rownames(object@fdata))) {
    errors <- c(errors, "'@fdata' slot must have row names.")
  }
  
  if (ncol(object@fdata) > 0 && is.null(colnames(object@fdata))) {
    errors <- c(errors, "'@fdata' slot must have column names.")
  }
  
  # fdata <-> nodes consistency
  if (nrow(object@nodes) > 0 && nrow(object@fdata) > 0) {
    if (!identical(rownames(object@nodes), rownames(object@fdata))) {
      errors <- c(errors, "Row names in '@fdata' slot must match row names in '@nodes' slot.")
    }
  }
  
  # graph <-> nodes consistency
  # Note: do not assume that @nodes rows and @graph vertices share the same order,
  # as igraph accessors are independent. Downstream code relies on validated
  # index values rather than positional order
  if (!inherits(object@graph, "igraph")) {
    errors <- c(errors, "'@graph' slot must be an igraph object.")
  } else {
    if(igraph::vcount(object@graph) > 0){
      g_vertex_names <- igraph::V(object@graph)$name
      if (is.null(g_vertex_names)) {
        errors <- c(errors, "'@graph' slot must have a 'name' attribute.")
      } else if (nrow(object@nodes) > 0) {
        if (!setequal(rownames(object@nodes), g_vertex_names)) {
          errors <- c(errors, 
            "Vertex names in '@graph' slot must match row names in '@nodes' slot.")
        }
      }
    }
  }
  
  # image <-> canvas consistency
  # @canvas is always derived from @image; a populated canvas without a source
  # image indicates an invalid object state.
  canvas_has_content <- .hasSlot(object, "canvas") && prod(dim(object@canvas)) > 1
  image_has_content  <- .hasSlot(object, "image")  && prod(dim(object@image))  > 1
  
  if (canvas_has_content && !image_has_content) {
    errors <- c(errors,
      "'@canvas' is populated but '@image' is empty; canvas requires a source image.")
  }
  
  # pars$image.space <-> canvas consistency
  # When normalization ran with image.space = TRUE the canvas must have been
  # populated. A mismatch indicates the object was modified outside the API.
  if (isTRUE(object@pars$image.space) && !canvas_has_content) {
    errors <- c(errors,
      "'pars$image.space' is TRUE but '@canvas' is empty; re-run normalizeGraphSpace().")
  }
  
  if (length(errors) == 0) TRUE else errors
  
})

#-------------------------------------------------------------------------------
#' Generate a unique identifier for GraphSpace objects
#' 
#' This helper function creates a unique ID without relying on the R 
#' Random Number Generator (RNG), making it immune to `set.seed()`.
#' It combines the Process ID (PID), high-precision system time, and 
#' a system-level temporary identifier to ensure uniqueness across 
#' parallel processes and rapid sequential calls.
#' 
#' @return A character string containing a unique alphanumeric ID.
#' @keywords internal
.generate_gs_uuid <- function() {
  # Capture current PID (uniqueness across different R sessions)
  pid <- Sys.getpid()
  
  # Capture high-precision time
  # We use %OS6 for microsecond precision
  time_stmp <- format(Sys.time(), "%d%H%M%OS6")
  
  # Capture a system-level unique string
  # tempfile() calls the OS to generate a unique name, bypassing R's RNG
  sys_id <- basename(tempfile(pattern = ""))
  
  # Combine and sanitize
  raw_id <- paste0("gs", pid, time_stmp, sys_id)
  uuid <- gsub("[^a-zA-Z0-9]", "", raw_id)
  
  return(uuid)
  
}

#-------------------------------------------------------------------------------

setGeneric("updateGraphSpace", function(x, ...) standardGeneric("updateGraphSpace"))

#' @title Update a GraphSpace object
#' @description Updates \code{GraphSpace} objects serialized from
#' previous package versions, adding any missing slots with default values.
#' @param x A \code{GraphSpace} object.
#' @param verbose Logical; if \code{TRUE}, reports which slots were added.
#' @return An updated \code{GraphSpace} object.
#' @aliases updateGraphSpace
#' @rdname updateGraphSpace
#' @export
setMethod("updateGraphSpace", "GraphSpace", function(x, verbose = TRUE) {
  .update_gs(x, verbose = verbose)
})

#' @keywords internal
.update_gs <- function(gs, verbose = TRUE) {

  new_slots <- c("image", "fdata", "uuid", "canvas")
  missing_slots <- new_slots[!sapply(new_slots, function(s) .hasSlot(gs, s))]
  
  if (length(missing_slots) == 0){
    return(gs)
  }
  
  if(verbose){
    if(.has_image(gs) || .has_fdata(gs)){
      rlang::warn(c(
        "!" = "Outdated 'GraphSpace' object detected.",
        "x" = paste0("Missing slot(s): ", paste(missing_slots, collapse = ", "), "."),
        "x" = "Image, canvas, and feature data cannot be safely reused and were reset to empty.",
        "i" = "Preserved unchanged: nodes, edges, graph, parameters, misc.",
        "*" = "Rebuild the object from scratch to restore the original image and features."
      ))
    } else {
      rlang::inform(c(
        "!" = "Outdated 'GraphSpace' object updated to the latest version.",
        "i" = paste0("Slot(s) added with default values: ", 
          paste(missing_slots, collapse = ", "), "."),
        "i" = "Re-run normalizeGraphSpace() before plotting.",
        "*" = "To ensure full compatibility, rebuild the object from scratch."
      ))
    }
  }
  
  # Reset normalization
  gs@pars$image.space <- FALSE
  gs@pars$is.normalized <- FALSE
  
  x <- new("GraphSpace",
    nodes = gs@nodes,
    edges = gs@edges,
    graph = gs@graph,
    pars = gs@pars,
    misc = gs@misc,
    uuid = if (.hasSlot(gs, "uuid")) gs@uuid else .generate_gs_uuid()
  )
  
  validObject(x)
  
  x
  
}

#-------------------------------------------------------------------------------
#' @keywords internal
.check_outdated_gs <- function(gs, slots = c("image", "canvas", "fdata", "uuid"),
  type = c("warn", "abort")) {
  
  type <- match.arg(type)
  
  check <- vapply(slots, function(s) .hasSlot(gs, s), logical(1))
  
  if (!all(check)) {
    msg <- c(
      "x" = paste0("Outdated 'GraphSpace' object, missing slot(s): ",
            paste(slots[!check], collapse = ", "), "."),
      "i" = "Run 'updateGraphSpace(x)' to migrate the object."
    )
    if (type == "abort") rlang::abort(msg) else rlang::warn(msg)
  }
  
  invisible(all(check))
}

#-------------------------------------------------------------------------------
setGeneric("summary", function(object, ...) standardGeneric("summary"))

#-------------------------------------------------------------------------------
#' @title Summarise a GraphSpace object
#'
#' @description Prints a structured summary of a \code{GraphSpace} object,
#' including graph topology, optional feature data, and spatial boundaries
#' for nodes and, when present, the background image.
#'
#' Node boundaries are always drawn from \code{@graph} (original pixel
#' coordinates, never modified). Image boundaries reflect \code{@canvas}
#' after normalization with \code{image.space = TRUE}, and \code{@image}
#' otherwise. When normalized, both boundary lines show the source range
#' and \code{[0,1]} target to make the transformation explicit.
#'
#' @param object A \code{GraphSpace} object.
#' @param ... Currently unused; present for S4 generic compatibility.
#'
#' @return Invisibly returns \code{object}, allowing the call to be used inside
#' a pipeline without side effects beyond the printed output.
#'
#' @seealso \code{\link{GraphSpace}}, \code{\link{normalizeGraphSpace}}
#' @importFrom igraph print.igraph
#' @aliases summary,GraphSpace-method
#' @exportMethod summary
setMethod("summary", "GraphSpace",
  function(object, ...) {
    
    igraph::print.igraph(object@graph, full = FALSE)
    
    if (.hasSlot(object, "fdata")) {
      nfeat <- ncol(object@fdata)
      if (nfeat > 0) {
        feat <- .gs_preview(colnames(object@fdata))
        cat("+ features: ", nfeat, " (", feat, ")\n", sep = "")
        nsamp <- nrow(object@fdata)
        samp <- .gs_preview(rownames(object@fdata), 2)
        cat("+ samples: ", nsamp, " (", samp, ")\n", sep = "")
      }
    }
    
    .inform_node_coord_status(object)
    
    .inform_boundaries( .node_boundaries(.get_nodes(object@graph)),
      if (.is_normalized(object)) list(x = c(0,1), y = c(0,1)) else NULL )
    
    if (.has_image(object)) {
      .inform_image_coord_status(object)
      img <- if (.has_canvas(object) && .is_image_space(object)) 
        object@canvas else object@image
      .inform_boundaries( .image_boundaries(object@image),
        if (.is_image_space(object)) .image_boundaries(img) else NULL )
    }
    
    .check_outdated_gs(object)
    
    invisible(object)
  }
)

#' @keywords internal
#' @importFrom utils head
.gs_preview <- function(x, n = 4) {
  if (length(x) == 0) return("<empty>")
  out <- head(x, n)
  if (length(x) > n) out <- c(out, "...")
  paste(out, collapse = ", ")
}

#-------------------------------------------------------------------------------
# show: header only; delegates content to summary()
setMethod("show", "GraphSpace", 
  function(object) {
    cat("A GraphSpace-class object for:\n")
    summary(object)
    invisible(object)
  }
)

#-------------------------------------------------------------------------------
# display helpers -- write directly to stdout, for show()/summary() only
#' @keywords internal
.inform_boundaries <- function(bounds, target = NULL) {
  suffix_x <- if (!is.null(target)) paste0(" -> [", target$x[1], ", ", target$x[2], "]") else ""
  suffix_y <- if (!is.null(target)) paste0(" -> [", target$y[1], ", ", target$y[2], "]") else ""
  cat("| x: [", bounds$x[1], ", ", bounds$x[2], "]", suffix_x, " (cols)\n", sep = "")
  cat("| y: [", bounds$y[1], ", ", bounds$y[2], "]", suffix_y, " (rows)\n", sep = "")
}

#' @keywords internal
.node_boundaries <- function(nodes) {
  if(nrow(nodes)>0){
    l <- list(
      x = c(floor(min(nodes$x, na.rm = TRUE)), ceiling(max(nodes$x, na.rm = TRUE))),
      y = c(floor(min(nodes$y, na.rm = TRUE)), ceiling(max(nodes$y, na.rm = TRUE)))
    )
  } else {
    l <- list(x = c(NaN,NaN), y = c(NaN,NaN))
  }
  l
}

#' @keywords internal
.image_boundaries <- function(image) {
  d <- dim(image)
  list(x = c(1L, d[2L]), y = c(1L, d[1L]))
}

#' @keywords internal
.inform_node_coord_status <- function(object) {
  if (.is_normalized(object)) {
    if (.is_image_space(object)) {
      cat("+ node spatial boundaries: normalized to image space\n")
    } else {
      cat("+ node spatial boundaries: normalized to graph space\n")
    }
  } else {
    cat("+ node spatial boundaries: raw graph\n")
  }
}

#' @keywords internal
.inform_image_coord_status <- function(object) {
  if (.is_image_space(object)){
    cat("+ image spatial boundaries: cropped to graph space\n")
  } else {
    cat("+ image spatial boundaries: raw image\n")
  }
}

#-------------------------------------------------------------------------------
# condition helper -- emit suppressible messages during operations
#' @keywords internal
.inform_node_boundaries <- function(nodes) {
  bounds <- .node_boundaries(nodes)
  rlang::inform(c(
    "Node spatial boundaries:",
    "i" = sprintf("x: [%s, %s] (cols)", bounds$x[1], bounds$x[2]),
    "i" = sprintf("y: [%s, %s] (rows)", bounds$y[1], bounds$y[2])
  ))
}

# condition helper -- emit suppressible messages during operations
#' @keywords internal
.inform_image_boundaries <- function(image) {
  bounds <- .image_boundaries(image)
  rlang::inform(c(
    "Image spatial boundaries:",
    "i" = sprintf("x: [%s, %s] (cols)", bounds$x[1], bounds$x[2]),
    "i" = sprintf("y: [%s, %s] (rows)", bounds$y[1], bounds$y[2])
  ))
}

#-------------------------------------------------------------------------------
#' @keywords internal
.has_canvas <- function(gs) {
  .hasSlot(gs, "canvas") && prod(dim(gs@canvas)) > 1
}

#' @keywords internal
.has_image <- function(gs) {
  .hasSlot(gs, "image") && prod(dim(gs@image)) > 1
}

#' @keywords internal
.has_fdata <- function(gs) {
  .hasSlot(gs, "fdata") && prod(dim(gs@fdata)) > 1
}

#' @keywords internal
.get_canvas <- function(gs) {
  if (.has_canvas(gs)) gs@canvas else gs@image
}

#' @keywords internal
.get_image <- function(gs) {
  gs@image
}

#' @keywords internal
.is_simplified <- function(gs){
  gs@pars$is.simplified %||% TRUE
}

#' @keywords internal
.is_normalized <- function(gs){
  gs@pars$is.normalized %||% FALSE
}

#' @keywords internal
.is_image_space <- function(gs){
  gs@pars$image.space %||% FALSE
}
