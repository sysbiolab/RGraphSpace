#' @importFrom methods setOldClass setClass
#' @importFrom grDevices as.raster
#' @importFrom igraph empty_graph
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
#' @slot image A \code{raster} object (see \code{\link[grDevices]{as.raster}}) used 
#' as background image.
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
  slot = c(
    nodes = "data.frame",
    edges = "data.frame",
    graph = "igraph",
    image = "raster",
    fdata = "Matrix",
    pars = "list",
    misc = "list",
    uuid = "character"
  ),
  prototype = list(
    nodes = data.frame(),
    edges = data.frame(),
    graph = igraph::empty_graph(),
    image = as.raster(matrix()),
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
    errors <- c(errors, "'@fdata' slot  must be a Matrix object.")
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
# show summary information on screen
setMethod("show", "GraphSpace", 
  function(object) {
    cat("A GraphSpace-class object for:\n")
    summary(object@graph)
    if (.hasSlot(object, "fdata")) {
      nfeat <- ncol(object@fdata)
      if (nfeat > 0) {
        feat <- .gs_preview(colnames(object@fdata))
        cat("+ features: ", nfeat, " (", paste(feat, collapse = ", "), ")\n", sep = "")
      }
    }
    invisible(object)
  }
)

#' @importFrom utils head
.gs_preview <- function(x, n = 4) {
  if (length(x) == 0) return("<empty>")
  out <- head(x, n)
  if (length(x) > n) out <- c(out, "...")
  paste(out, collapse = ", ")
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
setMethod("updateGraphSpace", "GraphSpace", function(x, verbose = FALSE) {
  .update_gs(x, verbose = verbose)
})

.update_gs <- function(gs, verbose = FALSE) {

  new_slots <- c("image", "fdata", "uuid")
  missing_slots <- new_slots[!sapply(new_slots, function(s) .hasSlot(gs, s))]
  
  if (length(missing_slots) == 0){
    if(verbose) rlang::inform("'GraphSpace' object is up to date.")
    return(gs)
  }
  
  rlang::warn(c(
    "Outdated 'GraphSpace' object: updating on the fly.",
    "i" = "Recently introduced feature slots may not be recoverable.",
    "i" = "Rebuild the object from scratch to fully restore all components."
  ))
  
  if (verbose){
    rlang::inform(paste0("Missing slot(s) added with defaults: ",
      paste(missing_slots, collapse = ", ")))
  }
  
  proto <- new("GraphSpace")
  x <- new("GraphSpace",
    nodes = gs@nodes,
    edges = gs@edges,
    graph = gs@graph,
    pars = gs@pars,
    misc = gs@misc,
    image = if (.hasSlot(gs, "image")) gs@image else proto@image,
    fdata = if (.hasSlot(gs, "fdata")) gs@fdata else proto@fdata,
    uuid  = if (.hasSlot(gs, "uuid"))  gs@uuid  else .generate_gs_uuid()
  )
  
  validObject(x)
  
}

#-------------------------------------------------------------------------------
.check_updated_gs <- function(gs, slots = c("image", "fdata", "uuid")) {
  
  check <- vapply(slots, function(s) .hasSlot(gs, s), logical(1))
  
  if (!all(check)) {
    rlang::abort(c(
      "x" = paste0(
        "Outdated 'GraphSpace' object: missing slot(s): ",
        paste(slots[!check], collapse = ", "),
        "."
      ),
      "i" = "Run 'updateGraphSpace(x)' to migrate the object."
    ))
  }
  
  invisible(TRUE)
  
}
