#' @importFrom methods setOldClass setClass
#' @importFrom grDevices as.raster
#' @importFrom igraph make_empty_graph
#' @importFrom tidygraph tbl_graph
methods::setOldClass("igraph")
methods::setOldClass("tbl_graph")
methods::setOldClass("gs_graph")

#-------------------------------------------------------------------------------
#' @title GraphSpace: An S4 class for igraph objects
#'
#' @slot nodes A data frame containing node coordinates, attributes, and metadata.
#' @slot edges  A data frame containing edge relationships and attributes.
#' @slot graph An \code{\link[igraph]{igraph}} object representing the graph 
#' structure.
#' @slot image A \code{\link[terra]{SpatRaster}} object holding the original 
#' background image as supplied by the user. Never modified after
#' construction; serves as the stable source for \code{normalizeGraphSpace()}.
#' @slot canvas A \code{\link[terra]{SpatRaster}} object holding the processed,
#' render-ready image produced by \code{normalizeGraphSpace()}. Receives all
#' centering, flipping, and margin adjustments. When this slot contains only
#' the empty sentinel, downstream accessors fall back to \code{@image}
#' automatically; see \link{gs_image}.
#' @slot fdata A \code{\link[Matrix]{Matrix}} object storing high-dimensional 
#' feature data associated with graph nodes.
#' @slot coords A data frame with raw coordinates. It also stores a raw  
#' \code{\link[sf]{sfc}} list column when a \code{geometry} is included 
#' by the \link{gs_geometry} function.
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
#' @importClassesFrom Matrix Matrix dgeMatrix
#' @exportClass GraphSpace
setClass("GraphSpace",
  slots = c(
    nodes = "data.frame",
    edges = "data.frame",
    graph = "igraph",
    image = "ANY",
    canvas = "ANY",
    fdata = "Matrix",
    coords = "data.frame",
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
    fdata = methods::new("dgeMatrix"),
    coords = data.frame(),
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
  
  if (!is.data.frame(object@coords)) {
    errors <- c(errors, "'@coords' slot must be a data.frame.")
  }
  
  if (nrow(object@coords) > 0 && is.null(rownames(object@coords))) {
    errors <- c(errors, "'@coords' slot must have row names.")
  }
  
  if (ncol(object@coords) > 0 && is.null(colnames(object@coords))) {
    errors <- c(errors, "'@coords' slot must have column names.")
  }
  
  # fdata <-> nodes consistency
  if (nrow(object@nodes) > 0 && nrow(object@fdata) > 0) {
    if (!identical(rownames(object@nodes), rownames(object@fdata))) {
      errors <- c(errors, 
        "Row names in '@fdata' slot must match row names in '@nodes' slot.")
    }
  }
  
  # coords <-> nodes consistency
  if (nrow(object@nodes) > 0 && nrow(object@coords) > 0) {
    if (!identical(rownames(object@nodes), rownames(object@coords))) {
      errors <- c(errors, 
        "Row names in '@coords' slot must match row names in '@nodes' slot.")
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
            "Vertex names in '@graph' slot must match row names in '@nodes'.")
        }
      }
    }
  }
  
  # image/canvas type
  # @image and @canvas are typed 'ANY' so that terra need not be present at
  # class-definition time; enforce the accepted representations here instead.
  if (!.gs_valid_image_source(object@image)) {
    errors <- c(errors, sprintf(
      "'@image' slot must be a raster, a SpatRaster, or empty; got '%s'.",
      paste(class(object@image), collapse = "/")))
  }
  if (!.gs_valid_image_source(object@canvas)) {
    errors <- c(errors, sprintf(
      "'@canvas' slot must be a raster, a SpatRaster, or empty; got '%s'.",
      paste(class(object@canvas), collapse = "/")))
  }
  
  # image <-> canvas consistency
  # @canvas is always derived from @image; a populated canvas without a source
  # image indicates an invalid object state.
  canvas_has_content <- .hasSlot(object, "canvas") && 
    .gs_image_has_content(object@canvas)
  image_has_content  <- .hasSlot(object, "image")  && 
    .gs_image_has_content(object@image)
  
  if (canvas_has_content && !image_has_content) {
    errors <- c(errors, paste(
      "'@canvas' is populated but '@image' is empty;",
      "canvas requires a source image.")
    )
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
# A valid image source under the 'ANY'-typed @image/@canvas slots.
.gs_valid_image_source <- function(x) {
  is.null(x) || inherits(x, "raster") || inherits(x, "SpatRaster")
}

# TRUE when the slot holds a populated image (not NULL, not the 1x1 sentinel).
.gs_image_has_content <- function(x) {
  if (is.null(x)) return(FALSE)
  d <- dim(x)
  !is.null(d) && prod(d) > 1
}

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

setGeneric("updateGraphSpace", function(x, ...) 
  standardGeneric("updateGraphSpace"))

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

  new_slots <- c("image", "fdata", "uuid", "canvas", "coords")
  missing_slots <- new_slots[!sapply(new_slots, function(s) .hasSlot(gs, s))]
  
  if (length(missing_slots) == 0){
    return(gs)
  }
  
  reset_image_fdata <- .has_image(gs) || .has_fdata(gs)
  if(length(missing_slots)==1 && missing_slots=="coords"){
    reset_image_fdata <- FALSE
  }

  if(verbose){
    
    if(reset_image_fdata){
      msg <- c(
        "!" = "Outdated 'GraphSpace' object detected.",
        "x" = paste0("Missing slot(s): ", paste(shQuote(missing_slots), 
          collapse = ", "), "."),
        "x" = paste0("Image, canvas, and feature data cannot ",
          "be safely reused and were reset to empty."),
        "*" = "To ensure full compatibility, rebuild the object from scratch."
      )
    } else {
      msg <- c("'GraphSpace' object updated to the latest version.",
        "i" = paste0("Slot(s) added with default values: 'misc', 'pars', ", 
          paste(shQuote(missing_slots), collapse = ", "), "."),
        "*" = "If normalized, re-run 'normalizeGraphSpace(...)' before plotting.")
    }
    
    cname <- class(gs)
    if(cname!="GraphSpace"){
      msg <- c(msg, 
        "!" = sprintf(
          "The '%s' extension will need to be rebuilt afterward.", 
          cname))
    }
    
    if(reset_image_fdata){
      rlang::warn(msg)
    } else {
      rlang::inform(msg)
    }
    
  }

  # Build new GraphSpace
  pars <- list(
    scale.factor = 1,
    is.directed = igraph::is_directed(gs@graph), 
    is.simplified = gs@pars$is.simplified %||% FALSE,
    is.normalized = FALSE, 
    image.space = FALSE,
    image.maxpixels = gs@pars$image.maxpixels %||% 4e6
  )
  x <- new("GraphSpace",
    graph = gs@graph,
    nodes = gs@nodes,
    edges = gs@edges,
    pars = pars
  )
  if (.hasSlot(gs, "uuid")){
    x@uuid <- gs@uuid
  } else {
    x@uuid <- .generate_gs_uuid()
  }
  if (.hasSlot(gs, "coords")){
    x@coords <- gs@coords
  } else {
    x@coords <- gs@nodes[, c("x", "y")]
  }
  if(!reset_image_fdata){
    if (.hasSlot(gs, "image")) x@image <- gs@image
    if (.hasSlot(gs, "fdata")) x@fdata <- gs@fdata
  }
  
  # Reset and denormalize, from @graph to @coords
  gs_scale_factor(x) <- 1
  
  validObject(x)
  
  x
  
}

#-------------------------------------------------------------------------------
#' @keywords internal
.check_outdated_gs <- function(gs, slots = NULL, type = c("warn", "abort")) {
  
  if(is.null(slots))  slots <- slotNames(new("GraphSpace"))
  
  type <- match.arg(type)
  
  check <- vapply(slots, function(s) .hasSlot(gs, s), logical(1))
  
  if (!all(check)) {
    cname <- class(gs)
    if(cname == "GraphSpace"){
      msg <- c("x" = paste0("Outdated 'GraphSpace' object, missing slot(s): ",
        paste(slots[!check], collapse = ", "), "."),
        "i" = "Run 'updateGraphSpace(x)' to migrate the object.")
    } else {
      msg <- c(
        "x" = sprintf("Outdated '%s' object, missing slot(s): %s.",
          cname, paste(slots[!check], collapse = ", ")),
        "i" = "Run 'updateGraphSpace(x)' to migrate the base 'GraphSpace' slots.",
        "!" = sprintf("The '%s' extension will need to be rebuilt afterward.", cname)
      )
    }
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
    
    .payload_summary(object)
    
    .fdata_summary(object)
    
    .node_coord_summary(object)
    
    .image_coord_summary(object)
    
    .check_outdated_gs(object)
    
    invisible(object)
    
  }
)

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
#' @keywords internal
.payload_summary <- function(object){
  # attributes that live only in @nodes (not on the graph, e.g. list payloads)
  node_only <- setdiff(colnames(object@nodes),
    c(igraph::vertex_attr_names(object@graph), "vertex"))
  if (length(node_only) > 0) {
    cat("+ node payload: ", length(node_only),
      " (", .gs_preview(node_only), ")\n", sep = "")
  }
}

#-------------------------------------------------------------------------------
#' @keywords internal
.fdata_summary <- function(object){
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
}

#-------------------------------------------------------------------------------
#' @keywords internal
.node_coord_summary <- function(object) {
  if (.is_normalized(object)) {
    if (.is_image_space(object)) {
      cat("+ node spatial boundaries: normalized to image space\n")
    } else {
      cat("+ node spatial boundaries: normalized to graph space\n")
    }
  } else {
    cat("+ node spatial boundaries: raw graph\n")
  }
  if (.hasSlot(object, "coords")) {
    .inform_boundaries( .node_boundaries(object@coords),
      if (.is_normalized(object)) list(x = c(0,1), y = c(0,1)) else NULL )
  }
}

#-------------------------------------------------------------------------------
#' @keywords internal
.image_coord_summary <- function(object) {
  if (.has_image(object)) {
    if (.is_image_space(object)){
      cat("+ image spatial boundaries: cropped to graph space\n")
    } else {
      cat("+ image spatial boundaries: raw image\n")
    }
    img <- if (.has_canvas(object) && .is_image_space(object)) 
      object@canvas else object@image
    .inform_boundaries( .image_boundaries(object@image),
      if (.is_image_space(object)) .image_boundaries(img) else NULL )
  }
}

#-------------------------------------------------------------------------------
#' @keywords internal
#' @importFrom utils head
.gs_preview <- function(x, n = 4) {
  if (length(x) == 0) return("<empty>")
  out <- head(x, n)
  if (length(x) > n) out <- c(out, "...")
  paste(out, collapse = ", ")
}

#-------------------------------------------------------------------------------
# display helpers -- write directly to stdout, for show()/summary() only
#' @keywords internal
.inform_boundaries <- function(bounds, target = NULL) {
  suffix_x <- if (!is.null(target)) paste0(" -> [", target$x[1], ", ", target$x[2], "]") else ""
  suffix_y <- if (!is.null(target)) paste0(" -> [", target$y[1], ", ", target$y[2], "]") else ""
  cat("| x: [", bounds$x[1], ", ", bounds$x[2], "]", suffix_x, " (cols)\n", sep = "")
  cat("| y: [", bounds$y[1], ", ", bounds$y[2], "]", suffix_y, " (rows)\n", sep = "")
}

#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
#' @keywords internal
.image_boundaries <- function(image) {
  d <- dim(image)
  list(x = c(1L, d[2L]), y = c(1L, d[1L]))
}

#-------------------------------------------------------------------------------
#' @keywords internal
.has_canvas <- function(gs) {
  .hasSlot(gs, "canvas") && .gs_image_has_content(gs@canvas)
}

#' @keywords internal
.has_image <- function(gs) {
  .hasSlot(gs, "image") && .gs_image_has_content(gs@image)
}

#' @keywords internal
.has_fdata <- function(gs) {
  .hasSlot(gs, "fdata") && prod(dim(gs@fdata)) > 0
}

#' @keywords internal
.get_canvas <- function(gs) {
  if (.has_canvas(gs)) return(gs@canvas)
  gs@image
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
.is_raw <- function(gs) {
  !.is_normalized(gs)
}

#' @keywords internal
.is_image_space <- function(gs){
  gs@pars$image.space %||% FALSE
}

#' @keywords internal
.is_directed <- function(gs){
  gs@pars$is.directed %||% FALSE
}
