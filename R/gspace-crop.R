
#-------------------------------------------------------------------------------
#' Crop node coordinates to graph and image spaces
#' 
#' Accessory function to crop a normalized \link{GraphSpace} object.
#' 
#' @param gs A normalized \code{GraphSpace} object.
#' @param xmin A single number in \code{[0,1]} specifying the lower x-boundary
#' of the plotting area.
#' @param xmax A single number in \code{[0,1]} specifying the upper x-boundary
#' of the plotting area.
#' @param ymin A single number in \code{[0,1]} specifying the lower y-boundary
#' of the plotting area.
#' @param ymax A single number in \code{[0,1]} specifying the upper y-boundary
#' of the plotting area.
#' @param crop.coord Deprecated from RGraphSpace 1.5.1; use cropping boundaries 
#' instead.
#' 
#' @details 
#' This function subsets a normalized graph space to a specific region defined
#' by the cropping boundaries. It recalculates node positions and background 
#' image boundaries to maintain spatial consistency after cropping.
#' 
#' @return A \code{GraphSpace} object with updated \code{nodes} 
#' and \code{image} slots.
#' 
#' @note This is an accessory function typically called during 
#' the preprocessing of \code{GraphSpace} objects before rendering.
#' 
#' @seealso \code{\link{normalizeGraphSpace}}
#' 
#' @examples
#' library(RGraphSpace)
#' library(igraph)
#' 
#' # Create a star graph
#' gtoy1 <- make_full_graph(30)
#' 
#' # Create a GraphSpace
#' gs <- GraphSpace(gtoy1)
#' 
#' gs <- normalizeGraphSpace(gs)
#' 
#' gs_crop <- cropGraphSpace(gs, ymax = 0.5)
#' 
#' plotGraphSpace(gs, add.labels = TRUE)
#' 
#' plotGraphSpace(gs_crop, add.labels = TRUE)
#' 
#' @aliases cropGraphSpace
#' @rdname cropGraphSpace-methods
#' @export
setMethod("cropGraphSpace", "GraphSpace", 
  function(gs, xmin = 0, xmax = 1, ymin = 0, ymax = 1, 
    crop.coord = deprecated()){
    
    gs <- updateGraphSpace(gs)
    
    if (lifecycle::is_present(crop.coord)) {
      lifecycle::deprecate_warn("1.5.1", "normalizeGraphSpace(crop.coord)",
        with = "normalizeGraphSpace(...)")
    }
    if(.is_numericVector(xmin) && length(xmin)==4){
      rlang::abort(
        message = c(
          "The 'crop.coord' argument is deprecated.",
          "i" = "Please use 'xmin', 'xmax', 'ymin', and 'ymax' instead."
        )
      )
    }
    
    if(!gs@pars$is.normalized){
      rlang::abort(
        message = c(
          "The 'GraphSpace' object must be normalized before cropping.",
          "i" = "Please run 'normalizeGraphSpace(gs)' first."
        )
      )
    }
    
    .validate_gs_args("singleNumber", "xmin", xmin)
    .validate_gs_args("singleNumber", "xmax", xmax)
    .validate_gs_args("singleNumber", "ymin", ymin)
    .validate_gs_args("singleNumber", "ymax", ymax)
    
    if(xmin >= xmax || ymin >= ymax){
      rlang::abort("'crop' must satisfy xmin < xmax and ymin < ymax.")
    }
    
    gs <- .crop_gspace(gs, crop.coord = c(xmin, xmax, ymin, ymax))
    
    return(gs)
    
  })


################################################################################
### Crop graph and image
################################################################################

#-------------------------------------------------------------------------------
.crop_gspace <- function(gs, crop.coord) {
  if (.has_image(gs)) {
    gs <- .crop_gspace_image(gs, crop.coord)
  } else {
    gs <- .crop_gspace_graph(gs, crop.coord)
  }
  return(gs)
}

#-------------------------------------------------------------------------------
.crop_gspace_graph <- function(gs, crop.coord) {
  
  xmin <- crop.coord[1]; xmax <- crop.coord[2]
  ymin <- crop.coord[3]; ymax <- crop.coord[4]
  
  # Crop nodes
  nodes <- gs@nodes
  cx <- nodes$x >= xmin & nodes$x <= xmax
  cy <- nodes$y >= ymin & nodes$y <= ymax
  nodes <- nodes[which(cx & cy), ]
  gs@nodes <- nodes
  
  gs <- .trim_graph_space(gs, nodes)
  
  return(gs)
  
}

#-------------------------------------------------------------------------------
.crop_gspace_image <- function(gs, crop.coord) {
  
  xmin <- crop.coord[1]; xmax <- crop.coord[2]
  ymin <- crop.coord[3]; ymax <- crop.coord[4]
  
  # Filter nodes within the crop window
  # Node: crop required normalized image
  nodes <- gs@nodes
  canvas <- gs@canvas
  
  # Compute image crop indices
  nrow_mat <- nrow(canvas)
  ncol_mat <- ncol(canvas)
  col_s <- max(1L, ceiling(xmin * ncol_mat))
  col_e <- min(ncol_mat, floor(xmax * ncol_mat))
  row_s <- max(1L, ceiling((1 - ymax) * nrow_mat))
  row_e <- min(nrow_mat, floor((1 - ymin) * nrow_mat))
  d <- c(row_e - row_s + 1L, col_e - col_s + 1L)
  
  # Reverse pixel-center encoding to recover 1-based pixel indices
  nodes$x <- .rescale_direct_inv(nodes$x, ncol_mat, 0.5 / ncol_mat) - (col_s - 1)
  nodes$y <- .rescale_direct_inv(nodes$y, nrow_mat, 0.5 / nrow_mat) - (nrow_mat - row_e)
  
  # Re-encode to pixel centers in the cropped image
  nodes$x <- .rescale_direct(nodes$x, d[2], 0.5 / d[2])
  nodes$y <- .rescale_direct(nodes$y, d[1], 0.5 / d[1])
  
  # Crop nodes
  cx <- nodes$x >= 0 & nodes$x <= 1
  cy <- nodes$y >= 0 & nodes$y <= 1
  nodes <- nodes[which(cx & cy), ]
  
  # Crop image
  gs@canvas <- canvas[row_s:row_e, col_s:col_e, drop = FALSE]
  gs <- .trim_graph_space(gs, nodes)
  gs <- .adjust_box_canvas(gs)
  return(gs)
  
}

#-------------------------------------------------------------------------------
.adjust_box_canvas <- function(gs){
  canvas <- gs@canvas
  nodes <- gs@nodes
  d <- dim(canvas)
  if(d[1] > d[2]){
    n <- ceiling( (d[1] - d[2])/2 )
    img_d <- matrix(NA, nrow = d[1], ncol = d[1])
    img_d[ , seq(n + 1, n + d[2])] <- as.matrix(canvas)
    canvas  <- as.raster(img_d)
    s <- n/d[1]
    nodes$x <- scales::rescale(nodes$x, from = c(0, 1), to = c(s, 1 - s))
  } else if(d[1] < d[2]){
    n <- ceiling( (d[2] - d[1])/2 )
    img_d <- matrix(NA, nrow = d[2], ncol = d[2])
    img_d[seq(n + 1, n + d[1]), ] <- as.matrix(canvas)
    canvas  <- as.raster(img_d)
    s <- n/d[2]
    nodes$y <- scales::rescale(nodes$y, from = c(0, 1), to = c(s, 1 - s))
  }
  gs@canvas <- canvas
  gs@nodes <- nodes
  return(gs)
}
