
#-------------------------------------------------------------------------------
#' Convert objects to GraphSpace
#' 
#' S3 generic function for coercing objects into a \code{GraphSpace} object.
#' 
#' @details
#' Unified entry point for converting graph, spatial, and high-dimensional 
#' data into a \code{GraphSpace} object.
#' 
#' Graph objects are imported either through native methods or via
#' \link[tidygraph]{as_tbl_graph} when available.
#' 
#' @param x An object to be converted.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' A \code{GraphSpace} object.
#' 
#' @seealso
#' \code{\linkS4class{GraphSpace}}
#' 
#' @export
as.GraphSpace <- function(x, ...) {
  UseMethod("as.GraphSpace")
}

#-------------------------------------------------------------------------------
#' @rdname as.GraphSpace
#' @importFrom tidygraph as_tbl_graph
#' @export
as.GraphSpace.default <- function(x, ...) {
  
  y <- tryCatch( as_tbl_graph(x),
    error = function(e) NULL
  )
  
  if (!is.null(y)) {
    return(GraphSpace(y, ...))
  }
  
  rlang::abort(paste0(
    "No 'as.GraphSpace' method available for class ",
    paste(class(x), collapse = ", "),
    ", and conversion via 'tidygraph::as_tbl_graph()' failed."
  ))
  
}

#-------------------------------------------------------------------------------
#' @rdname as.GraphSpace
#' @export
as.GraphSpace.igraph <- function(x, ...) {
  GraphSpace(x, ...)
}

#-------------------------------------------------------------------------------
#' @rdname as.GraphSpace
#' @export
as.GraphSpace.tbl_graph <- function(x, ...) {
  GraphSpace(x, ...)
}

#-------------------------------------------------------------------------------
#' @rdname as.GraphSpace
#' @export
as.GraphSpace.data.frame <- function(x, ...) {
  GraphSpace(x, ...)
}

#-------------------------------------------------------------------------------
#' @rdname as.GraphSpace
#'
#' @param space Character specifying the coordinate space used for node
#' geometry. Either \code{"embedding"} or \code{"spatial"}. See details.
#' @param layer Name of the layer from which node data should be retrieved
#' (see \code{\link[SeuratObject]{LayerData}}).
#' @param ... Additional arguments passed to methods associated with the
#' selected \code{space}.
#' 
#' @details
#' For \strong{Seurat} objects, coordinate extraction depends on the 
#' selected \code{space}:
#' \itemize{
#'   \item \code{space = "embedding"} uses the first two dimensions returned by
#'   \code{\link[SeuratObject]{Embeddings}}.
#'   
#'   \item \code{space = "spatial"} uses tissue coordinates returned by
#'   \code{\link[SeuratObject]{GetTissueCoordinates}}.
#' }
#' 
#' Assay data are stored in the \code{data} slot of the resulting
#' \code{GraphSpace} object. Node metadata from \code{x@meta.data} are
#' appended to the node table.
#' 
#' @export
as.GraphSpace.Seurat <- function(x, 
  space = c("embedding","spatial"), 
  layer = NULL, ...) {
  
  space <- match.arg(space)
  
  if (!requireNamespace("SeuratObject", quietly = TRUE)) {
    rlang::abort("Package 'SeuratObject' is required for Seurat conversion.")
  }
  
  if (!inherits(x, "Seurat")){
    rlang::abort("'x' must be a Seurat object.")
  }
  
  # Get node coordinates
  if (space == "embedding"){
    
    coords <- SeuratObject::Embeddings(x, ...)
    
    if (is.null(coords) || length(dim(coords)) != 2L ||
        nrow(coords) == 0L || ncol(coords) < 2L) {
      rlang::abort(
        "Reduction must contain at least two dimensions."
      )
    }
    coords <- coords[, seq_len(2), drop = FALSE]
    colnames(coords) <- c("x", "y")
    
  } else {
    
    if (length(SeuratObject::Images(x)) == 0){
      rlang::abort("No spatial coordinates found.")
    }
    
    coords <- SeuratObject::GetTissueCoordinates(object = x, ...)
    
    if (is.null(coords) || (!is.matrix(coords) && !is.data.frame(coords)) ) {
      rlang::abort(
        "Spatial coordinates must be returned as a matrix or data frame."
      )
    }
    
    # Remove unnamed columns occasionally returned by some methods
    coords[, !nzchar(colnames(coords))] <- NULL
    
    if (nrow(coords) == 0L || ncol(coords) < 2L){
      rlang::abort(
        "No valid spatial coordinates found."
      )
    }
    
    if ( !all(c("x", "y") %in% colnames(coords))){
      rlang::abort(
        "Spatial coordinates must contain 'x' and 'y' columns."
      )
    }
    
  }
  
  if (is.null(rownames(coords))) {
    if (nrow(coords) != ncol(x)) {
      rlang::abort(c(
        "Unable to infer node identifiers from coordinate space.",
        "i" = "Coordinate rownames are missing.",
        "x" = "The number of coordinates does not match the number of samples in 'x'."
      ))
    }
    rownames(coords) <- colnames(x)
  }
  coords <- as.data.frame(coords)
  
  # Add metadata
  metadata <- x[[]]
  if(inherits(metadata, "data.frame") && ncol(metadata) > 0){
    cids <- setdiff(colnames(metadata), colnames(coords))
    if (length(cids) > 0){
      metadata <- metadata[rownames(coords), cids, drop = FALSE]
      coords <- cbind(coords, metadata)
    }
  }
  
  # Create GraphSpace
  coords$nodeSize <- 1
  gs <- GraphSpace(coords, verbose = FALSE)
  
  # Add fdata
  fdata <- SeuratObject::LayerData(x, layer = layer)
  fdata <- Matrix::t(fdata)
  gs_fdata(gs) <- fdata
  
  .inform_data_specs(fdata, layer, space, ...)
  
  .inform_coord_boundaries(coords)
  
  return(gs)
  
}

#-------------------------------------------------------------------------------
# Informative messages on data dimensions, image specs, and coordinate checks
#-------------------------------------------------------------------------------

.inform_data_specs <- function(fdata, layer, space, ...) {
  
  args <- list(...)
  d <- dim(fdata)
  
  args_str <- if (length(args) > 0) {
    paste(mapply(function(k, v) sprintf("%s=%s", k, deparse(v)),
      names(args), args), collapse = ", ")
  } else NULL
  
  msg <- c(
    "Seurat object converted to GraphSpace:",
    "i" = sprintf("space=%s, layer=%s, features=%s, samples=%s%s",
      space, 
      if (is.null(layer)) "default" else layer, d[2], d[1],
      if (!is.null(args_str)) sprintf(", %s", args_str) else "")
  )
  
  rlang::inform(msg)
}

.inform_coord_boundaries <- function(coords) {
  xr <- c(floor(min(coords$x, na.rm = TRUE)), ceiling(max(coords$x, na.rm = TRUE)))
  yr <- c(floor(min(coords$y, na.rm = TRUE)), ceiling(max(coords$y, na.rm = TRUE)))
  
  rlang::inform(c(
    "Node spatial boundaries:",
    "i" = sprintf("x: [%s, %s] (cols)", xr[1], xr[2]),
    "i" = sprintf("y: [%s, %s] (rows)", yr[1], yr[2])
  ))
}

.inform_image_boundaries <- function(image) {
  
  d <- dim(image)
  
  rlang::inform(c(
    "Image spatial boundaries:",
    "i" = sprintf("x: [%s, %s] (cols)", 1, d[2]),
    "i" = sprintf("y: [%s, %s] (rows)", 1, d[1])
  ))
  
}


