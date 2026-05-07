setOldClass("igraph")
setOldClass("raster")

#' @title GraphSpace: An S4 class for igraph objects
#'
#' @slot nodes A data frame with xy-vertex coordinates.
#' @slot edges  A data frame with edges.
#' @slot graph An igraph object.
#' @slot image A raster background image matrix.
#' @slot pars A list with parameters.
#' @slot misc A list with intermediate objects for downstream methods.
#' @slot uuid An Universally Unique Identifier (UUID).
#' 
#' @method plotGraphSpace \code{\link{plotGraphSpace}}
#' @method getGraphSpace \code{\link{getGraphSpace}}
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
    pars = "list",
    misc = "list",
    uuid = "character"
  ),
  prototype = list(
    nodes = data.frame(),
    edges = data.frame(),
    graph = igraph::empty_graph(),
    image = as.raster(matrix()),
    pars = list(),
    misc = list(),
    uuid = character()
  )
)
setValidity("GraphSpace", function(object) {
  errors <- character()
  
  # Check if node and edge slots are data.frames
  if (!is.data.frame(object@nodes)) {
    errors <- c(errors, "'nodes' must be a data.frame.")
  }
  if (!is.data.frame(object@edges)) {
    errors <- c(errors, "'edges' must be a data.frame.")
  }
  
  # Check if graph is an igraph object
  if (!inherits(object@graph, "igraph")) {
    errors <- c(errors, "'graph' must be an igraph object.")
  }
  
  # If node names exist, check consistency with graph vertices
  if (nrow(object@nodes) > 0 && igraph::vcount(object@graph) > 0) {
    g_vertex_names <- igraph::V(object@graph)$name
    node_row_names <- rownames(object@nodes)
    
    if (!is.null(node_row_names)) {
      if (!setequal(g_vertex_names, node_row_names)) {
        errors <- c(errors, "Vertex names in 'graph' must match row names in 'nodes'.")
      }
    }
  }
  
  if (length(errors) == 0) TRUE else errors
})

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
