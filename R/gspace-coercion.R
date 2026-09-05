
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
    rlang::inform(paste0(
      "No native 'as.GraphSpace' method for class '", class(x)[1], "'; ",
      "converted via 'as_tbl_graph()'. Verify the resulting graph ",
      "structure if this wasn't the intended input."
    ))
    return(GraphSpace(y, ...))
  } else {
    y <- tryCatch( as.data.frame(x),
      error = function(e) NULL
    )
    if (!is.null(y)){
      rlang::inform(paste0(
        "No native 'as.GraphSpace' method for class '", class(x)[1], "'; ",
        "converted via 'as.data.frame()'. Verify the resulting graph ",
        "structure if this wasn't the intended input."
      ))
      return(GraphSpace(y, ...))
    }
  }
  
  rlang::abort(paste0(
    "No 'as.GraphSpace' method available for class ",
    paste(class(x), collapse = ", "),
    ", and conversion via 'as.data.frame()' failed."
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
#' @export
as.GraphSpace.DFrame <- function(x, ...) {
  rlang::inform(
    "Coercing 'DFrame' to 'GraphSpace' via 'as.data.frame()'..."
  )
  x <- as.data.frame(x)
  GraphSpace(x, ...)
}

#-------------------------------------------------------------------------------
#' @rdname as.GraphSpace
#' @export
as.GraphSpace.matrix <- function(x, ...) {
  rlang::inform(
    "Coercing 'matrix' to 'GraphSpace' via 'as.data.frame()'..."
  )
  x <- as.data.frame(x)
  GraphSpace(x, ...)
}

#-------------------------------------------------------------------------------
#' @param assay Name of the assay in the 
#' \code{\link[SpatialExperiment]{SpatialExperiment}} object from which 
#' data should be retrieved (see \code{\link[SummarizedExperiment]{assay}}).
#' @rdname as.GraphSpace
#' @export
as.GraphSpace.SpatialExperiment <- function(x, assay = "counts", ...) {
  
  rlang::inform("Coercing 'SpatialExperiment' to 'GraphSpace'...")
  
  if (!requireNamespace("SpatialExperiment", quietly = TRUE)) {
    rlang::abort("Package 'SpatialExperiment' is required for conversion.")
  }
  
  if (!inherits(x, "SpatialExperiment")){
    rlang::abort("'x' must be a SpatialExperiment object.")
  }
  
  # Extract coordinates data
  coordinates <- SpatialExperiment::spatialCoords(x)
  if (is.null(coordinates) || (!is.matrix(coordinates) && 
      !is.data.frame(coordinates)) ) {
    rlang::abort(
      "Spatial coordinates must be returned as a matrix or data frame."
    )
  }
  coordinates <- as.data.frame(coordinates)
  
  # Remove unnamed columns occasionally returned by some methods
  coordinates <- coordinates[, nzchar(colnames(coordinates)), drop = FALSE]
  if (nrow(coordinates) == 0L || ncol(coordinates) < 2L){
    rlang::abort(
      "No valid spatial coordinates found."
    )
  }
  
  if ( !all(c("x", "y") %in% colnames(coordinates)) ){
    if(ncol(coordinates)==2){
      colnames(coordinates) <- c("x", "y")
    } else {
      rlang::abort(
        "Spatial coordinates must contain 'x' and 'y' columns."
      )
    }
  }
  
  .check_id_alignment(
    ids = rownames(coordinates), ref_ids = colnames(x),
    id_label = "coordinate row names", 
    ref_label = "'colnames(x)' identifiers"
  )
  rownames(coordinates) <- colnames(x)
  
  # Extract col data
  cdata <- SummarizedExperiment::colData(x)
  if(inherits(cdata, "DFrame")) cdata <- as.data.frame(cdata)
  if(inherits(cdata, "data.frame") && ncol(cdata) > 0){
    cids <- setdiff(colnames(cdata), colnames(coordinates))
    if (length(cids) > 0){
      .check_id_alignment(
        ids = rownames(cdata), ref_ids = colnames(x),
        id_label = "data row names", 
        ref_label = "'colnames(x)' identifiers"
      )
      cdata <- cdata[ , cids, drop = FALSE]
      rownames(cdata) <- colnames(x)
      coordinates <- cbind(coordinates, cdata)
    }
  }
  
  # Create GraphSpace
  coordinates$nodeSize <- 1
  gs <- GraphSpace(coordinates)
  
  # Add fdata
  rlang::inform("Adding 'assay' data to the 'GraphSpace' object...")
  fdata <- as(SummarizedExperiment::assay(x, assay), "dgCMatrix")
  if (is.null(fdata)) {
    rlang::abort(c("x" = "assay() returned NULL.",
      "i" = sprintf("Assay '%s' may not exist.", 
        assay %||% "default")))
  }
  .check_id_alignment(
    ids = colnames(fdata), ref_ids = colnames(x),
    id_label = sprintf("'%s' assay col names", assay), 
    ref_label = "'colnames(x)' identifiers"
  )
  colnames(fdata) <- colnames(x)
  fdata <- Matrix::t(fdata)
  gs_fdata(gs) <- fdata
  
  return(gs)
  
}

#-------------------------------------------------------------------------------
#' @rdname as.GraphSpace
#'
#' @param layer Name of the layer in the \code{\link[SeuratObject]{Seurat}} object 
#' from which node data should be retrieved 
#' (see \code{\link[SeuratObject]{LayerData}}).
#' @param space Character specifying the coordinate space used for node
#' geometry. Either \code{"embedding"} or \code{"spatial"}. See details.
#' @param ... Additional arguments passed to coercion methods.
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
as.GraphSpace.Seurat <- function(x, layer = NULL,
  space = c("embedding","spatial"), ...) {
  
  space <- match.arg(space)
  
  rlang::inform("Coercing 'Seurat' to 'GraphSpace'...")
  
  if (!requireNamespace("SeuratObject", quietly = TRUE)) {
    rlang::abort("Package 'SeuratObject' is required for Seurat conversion.")
  }
  
  if (!inherits(x, "Seurat")){
    rlang::abort("'x' must be a Seurat object.")
  }
  
  # Get node coordinates
  if (space == "embedding"){
    
    coordinates <- SeuratObject::Embeddings(x, ...)
    
    if (is.null(coordinates) || length(dim(coordinates)) != 2L ||
        nrow(coordinates) == 0L || ncol(coordinates) < 2L) {
      rlang::abort(
        "Reduction must contain at least two dimensions."
      )
    }
    coordinates <- coordinates[, seq_len(2), drop = FALSE]
    colnames(coordinates) <- c("x", "y")
    
  } else {
    
    if (length(SeuratObject::Images(x)) == 0){
      rlang::abort("No spatial coordinates found.")
    }
    
    coordinates <- SeuratObject::GetTissueCoordinates(object = x, ...)
    
    if (is.null(coordinates) || (!is.matrix(coordinates) && 
        !is.data.frame(coordinates)) ) {
      rlang::abort(
        "Spatial coordinates must be returned as a matrix or data frame."
      )
    }
    coordinates <- as.data.frame(coordinates)
    
    # Remove unnamed columns occasionally returned by some methods
    coordinates <- coordinates[, nzchar(colnames(coordinates)), drop = FALSE]
    
    if (nrow(coordinates) == 0L || ncol(coordinates) < 2L){
      rlang::abort(
        "No valid spatial coordinates found."
      )
    }
    
    if ( !all(c("x", "y") %in% colnames(coordinates)) ){
      if(ncol(coordinates)==2){
        colnames(coordinates) <- c("x", "y")
      } else {
        rlang::abort(
          "Spatial coordinates must contain 'x' and 'y' columns."
        )
      }
    }

  }
  
  # Check 'coordinates' alignment; will preserve internal ordering 
  # with with 'x' default identifiers
  .check_id_alignment(
    ids = rownames(coordinates), ref_ids = colnames(x),
    id_label = "coordinate row names", 
    ref_label = "'colnames(x)' identifiers"
  )
  rownames(coordinates) <- colnames(x)
  
  # Add cdata
  cdata <- x[[]]
  if(inherits(cdata, "data.frame") && ncol(cdata) > 0){
    cids <- setdiff(colnames(cdata), colnames(coordinates))
    if (length(cids) > 0){
      .check_id_alignment(
        ids = rownames(cdata), ref_ids = colnames(x),
        id_label = "data row names", 
        ref_label = "'colnames(x)' identifiers"
      )
      cdata <- cdata[ , cids, drop = FALSE]
      rownames(cdata) <- colnames(x)
      coordinates <- cbind(coordinates, cdata)
    }
  }
  
  # Create GraphSpace
  coordinates$nodeSize <- 1
  gs <- GraphSpace(coordinates)
  
  # Add fdata
  rlang::inform("Adding 'layer' data to the 'GraphSpace' object...")
  fdata <- SeuratObject::LayerData(x, layer = layer)
  if (is.null(fdata)) {
    rlang::abort(c("x" = "LayerData() returned NULL.",
      "i" = sprintf("Layer '%s' may not exist.", 
        layer %||% "default")))
  }
  .check_id_alignment(
    ids = colnames(fdata), ref_ids = colnames(x),
    id_label = sprintf("'%s' layer col names", layer), 
    ref_label = "'colnames(x)' identifiers"
  )
  colnames(fdata) <- colnames(x)
  fdata <- Matrix::t(fdata)
  gs_fdata(gs) <- fdata
  
  return(gs)
  
}

#-------------------------------------------------------------------------------
.check_id_alignment <- function(ids, ref_ids,
  id_label = "identifiers",
  ref_label = "reference identifiers") {
  
  if (length(ids) != length(ref_ids)) {
    rlang::abort(c(
      sprintf("Unable to align %s with %s.", id_label, ref_label),
      "x" = sprintf("The number of %s does not match the number of %s.", 
        id_label, ref_label)
    ))
  }
  
  bl <- ids == ref_ids
  if (!all(bl)) {
    n <- sum(!bl)
    pct <- round(100 * n / length(bl))
    rlang::warn(c(
      sprintf("%d of %d (%d%%) %s did not match %s.", n, length(bl), 
        pct, id_label, ref_label),
      "i" = sprintf("%s will be overwritten with %s by position.", 
        id_label, ref_label),
      "!" = "If the two are not already in the same order, values will be misaligned."
    ))
  }
  
  invisible(TRUE)
}



