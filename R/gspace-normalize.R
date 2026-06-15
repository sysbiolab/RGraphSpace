
#-------------------------------------------------------------------------------
#' Normalize node coordinates to graph and image spaces
#' 
#' Accessory functions to normalize node coordinates in GraphSpace, 
#' either by centering them within the graph boundaries 
#' or by mapping them to pixel coordinates of a background image.
#' 
#' @param gs A \code{GraphSpace} object to be normalized.
#' @param mar A single numeric value in \code{[0, 0.5]} controlling the size of
#' the outer margins around the graph. Without an image, \code{mar} specifies
#' symmetric margins as a fraction of the graph space. With an image,
#' \code{mar} is interpreted as a fraction of the available image margins
#' surrounding the graph.
#' @param image.space Logical; if an image is available, whether to use it as 
#' a background reference map. When enabled, \code{x} and \code{y} graph 
#' coordinates are interpreted as pixel coordinates in the image matrix. 
#' Images can be inspected and assigned with \code{\link{gs_image}}.
#' @param flip.y Logical; whether to flip the node coordinates along the y-axis.
#' Useful for aligning nodes with image backgrounds, which often use an 
#' inverted coordinate system. Defaults to \code{image.space}.
#' @param flip.x Logical; whether to flip the node coordinates along the x-axis.
#' @param rotate.xy Logical; whether to rotate x-y coordinates.
#' @param flip.v Logical; whether to vertically flip the background image  
#' matrix (top-to-bottom) to align with the graph coordinate system.
#' @param flip.h Logical; whether to horizontally flip the background image  
#' matrix (left-to-right) to align with the graph coordinate system.
#' @param crop.coord An optional numeric vector of length four specifying a  
#' cropping region (xmin, xmax, ymin, ymax), with values in normalized 
#' coordinates \code{[0,1]}.
#' @param verbose A single logical value specifying to display detailed 
#' messages (when \code{verbose=TRUE}) or not (when \code{verbose=FALSE}).
#' @param use_image Deprecated from RGraphSpace 1.4.0; use 
#' \code{image.space} instead.
#' 
#' @details 
#' These functions provide different strategies for coordinate transformation:
#' \itemize{
#'   \item \bold{normalizeGraphSpace}: Re-scales node coordinates to a 
#'   \code{[0, 1]} unit square based on the graph's bounding box (when 
#'   \code{image.space = FALSE}) or maps them to pixel coordinates (when 
#'   \code{image.space = TRUE} and an image is provided; see \link{gs_image}). 
#'   It handles image-to-graph alignment via \code{flip.\*} and 
#'   \code{rotate.\*} arguments, used to adjust the graph origin with the 
#'   image matrix layout. Users should be aware of the potential discrepancy 
#'   between image matrix orientation (top-down) and graph coordinates 
#'   (bottom-up). The function attempts to automatically adjust the y-axis to 
#'   align the graph's bottom-up coordinates with the image's top-down layout, 
#'   but further manual adjustments might be required. 
#'   \item \bold{cropGraphSpace}: Subsets the normalized graph space into a 
#'   specific region defined by \code{crop.coord}. 
#'   It recalculates node positions and background image boundaries to maintain 
#'   spatial consistency after cropping. This function requires a previously 
#'   normalized \code{GraphSpace} object.
#' }
#' 
#' @return A \code{GraphSpace} object with updated \code{nodes} 
#' and \code{image} slots.
#' 
#' @note This is an accessory function typically called during 
#' the preprocessing of \code{GraphSpace} objects before rendering.
#' 
#' @seealso \code{\link{gs_image}}
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
#' gs_crop <- cropGraphSpace(gs, crop.coord = c(0, 1, 0, 0.5))
#' 
#' plotGraphSpace(gs, add.labels = TRUE)
#' 
#' plotGraphSpace(gs_crop, add.labels = TRUE)
#' 
#' @aliases normalizeGraphSpace
#' @rdname normalizeGraphSpace-methods
#' @export
setMethod("normalizeGraphSpace", "GraphSpace",
  function(gs, mar = 0.1, image.space = .has_image(gs), 
    flip.x = FALSE, flip.y = image.space, rotate.xy = FALSE, 
    flip.v = FALSE, flip.h = FALSE, verbose = TRUE, 
    use_image = deprecated()){
    
    if (lifecycle::is_present(use_image)) {
      deprecate_soft("1.4.0", "normalizeGraphSpace(use_image)")
      image.space <- .has_image(gs)
    }
    
    .validate_gs_args("singleLogical", "image.space", image.space)
    .validate_gs_args("singleLogical", "flip.v", flip.v)
    .validate_gs_args("singleLogical", "flip.h", flip.h)
    .validate_gs_args("singleLogical", "flip.x", flip.x)
    .validate_gs_args("singleLogical", "flip.y", flip.y)
    .validate_gs_args("singleLogical", "rotate.xy", rotate.xy)
    .validate_gs_args("singleLogical", "verbose", verbose)
    .validate_gs_args("singleNumber", "mar", mar)
    
    if (mar < 0 || mar > 0.5) {
      rlang::warn("'mar' should be in [0, 0.5]")
      mar <- max(0, min(mar, 0.5))
    }
    if(image.space && !gs@pars$image.layer){
      rlang::warn(
        message = c(
          "!" = "'image.space = TRUE' requested, but no image is available.",
          "i" = "Proceeding without the image layer.",
          "*" = "Use `gs_image()` to inspect the image slot.",
          "*" = "Use `gs_image()<-` to add an image."
        )
      )
      image.space <- FALSE
    }
    
    if(gs_vcount(gs)>0){
      if(image.space){
        gs <- .normalizeGraphSpace.image(gs, mar, flip.x, flip.y, 
          rotate.xy, flip.v, flip.h, verbose)
      } else {
        gs <- .normalizeGraphSpace.graph(gs, mar, flip.x, flip.y, 
          rotate.xy, verbose)
      }
    }
    
    return(gs)
    
  }
)

.has_image <- function(gs) {
  image.layer <- gs@pars$image.layer %||% FALSE
  img <- gs_image(gs)
  !is.null(img) && prod(dim(img)) > 1 && image.layer
}

.normalizeGraphSpace.graph <- function(gs, mar, flip.x, flip.y, 
  rotate.xy, verbose){
  
  nodes <- .get_nodes(gs@graph)
  nodes <- .setCoordToGraph(nodes, flip.x, flip.y, rotate.xy, verbose)
  if(verbose) message("Normalizing node coordinates to graph space...")
  gs@nodes <- .fit_graph_space(nodes, mar)
  gs@pars$image.space <- FALSE
  gs@pars$is.normalized <- TRUE
  gs@pars$flip.x <- flip.x
  gs@pars$flip.y <- flip.y
  gs@pars$rotate.xy <- rotate.xy
  gs@pars$mar <- mar
  
  return(gs)
  
}

.normalizeGraphSpace.image <- function(gs, mar, flip.x, flip.y, 
  rotate.xy, flip.v, flip.h, verbose){
  
  nodes <- .get_nodes(gs@graph)
  image <- gs_image(gs)
  
  if(verbose) message("Normalizing node coordinates to image space...")
  
  if(flip.v){
    if(verbose) message("Flipping image top-to-bottom...")  
    image <- image[rev(seq_len(nrow(image))), , drop = FALSE]
  } 
  
  if(flip.h){
    if(verbose) message("Flipping image left-to-right...")  
    image <- image[, rev(seq_len(ncol(image))), drop = FALSE]
  } 
  
  nodes <- .setCoordToImage(nodes, image, flip.x, flip.y, rotate.xy, verbose)
  
  l_temp <- .fitImageNodes(nodes, image, mar)
  
  gs@image <- l_temp$image
  gs@nodes <- l_temp$nodes
  gs@pars$is.normalized <- TRUE
  gs@pars$image.space <- TRUE
  gs@pars$flip.v <- flip.v
  gs@pars$flip.x <- flip.x
  gs@pars$flip.y <- flip.y
  gs@pars$rotate.xy <- rotate.xy
  gs@pars$mar <- mar
  
  return(gs)
}

#-------------------------------------------------------------------------------
#' @aliases cropGraphSpace
#' @rdname normalizeGraphSpace-methods
#' @export
setMethod("cropGraphSpace", "GraphSpace", 
  function(gs, crop.coord = c(0, 1, 0, 1), verbose = TRUE){
    
    .validate_gs_args("numeric_vec", "crop.coord", crop.coord)
    
    if(length(crop.coord)!=4){
      rlang::abort("'crop.coord' should be a numeric vector of length = 4.")
    }
    
    if(any(crop.coord < 0) || any(crop.coord > 1)){
      rlang::abort("'crop.coord' should be in [0,1].")
    }
    
    if(!gs@pars$is.normalized){
      rlang::abort(
        message = c(
          "The 'GraphSpace' object must be normalized before cropping.",
          "i" = "Please run 'normalizeGraphSpace(gs)' first."
        )
      )
    }
    
    gs <- .crop_gspace(gs, crop.coord)
    
    return(gs)
    
  })

################################################################################
### Graph adjusts
################################################################################
.setCoordToGraph <- function(nodes, flip.x = FALSE, flip.y = FALSE, 
  rotate.xy= FALSE, verbose = TRUE){
  
  # Rotated coordinates
  coord <- nodes[,c("x","y")]
  if(rotate.xy){
    if(verbose) message("Rotating xy-coordinates...")
    coord$x2 <- coord$y
    coord$y2 <- coord$x
  } else {
    coord$x2 <- coord$x
    coord$y2 <- coord$y
  }
  
  # Flip y-coordinates
  if(flip.y){
    if(verbose) message("Flipping y-coordinates...")
    y <- coord$y2
    coord$y2 <- -(y - max(y)) - max(y) + 1
  }
  
  # Flip x-coordinates
  if(flip.x){
    if(verbose) message("Flipping x-coordinates...")
    x <- coord$x2
    coord$x2 <- -(x - max(x)) - max(x) + 1
  }
  # Update coordinates
  nodes$x <- coord$x2
  nodes$y <- coord$y2
  
  return(nodes)
}

################################################################################
### Adjust node coordinates
################################################################################
# Fit graph in a [0, 1] space with focus on adjusting margins
.fit_graph_space <- function(nodes, mar = 0.1){
  
  mar <- max(0, min(mar, 0.49))
  
  nds <- nodes
  
  if(nrow(nds)>0){
    
    nds$x <- nds$x - mean(range(nds$x))
    nds$y <- nds$y - mean(range(nds$y))
    
    from <- range(c(nds$x, nds$y))
    to <- c(mar, 1 - mar)
    
    nds$x <- scales::rescale(nds$x, from = from, to=to)
    nds$y <- scales::rescale(nds$y, from = from, to=to)
    
  }
  
  return(nds)
}

################################################################################
### Graph-to-image adjusts
################################################################################
.setCoordToImage <- function(nodes, image, 
  flip.x = FALSE, flip.y = FALSE, rotate.xy = FALSE, 
  verbose = TRUE){
  
  # Rotated coordinates
  coords <- nodes[,c("x","y")]
  if(rotate.xy){
    if(verbose) message("Rotating xy-coordinates...")
    coords$x2 <- coords$y
    coords$y2 <- coords$x
  } else {
    coords$x2 <- coords$x
    coords$y2 <- coords$y
  }
  
  if(flip.y){
    if(verbose) message("Flipping y-coordinates...")
    y <- coords$y2
    y <- -(y - max(y)) + nrow(image) - max(y) + 1
    coords$y2 <- y
  }
  
  # Flip x-coordinates over image axis
  if(flip.x){
    if(verbose) message("Flipping x-coordinates...")
    x <- coords$x2
    x <- -(x - max(x)) + ncol(image) - max(x) + 1
    coords$x2 <- x
  }
  
  # Update coordinates
  .check_final_coords(coords, image)
  
  nodes$x <- coords$x2
  nodes$y <- coords$y2
  
  return(nodes)
  
}

#-------------------------------------------------------------------------------
.check_final_coords <- function(coords, image){
  
  d <- dim(image)
  xr <- range(coords$x2, na.rm = TRUE)
  yr <- range(coords$y2, na.rm = TRUE)
  
  xr_int <- c(floor(xr[1]), ceiling(xr[2]))
  yr_int <- c(floor(yr[1]), ceiling(yr[2]))
  
  out_x <- (xr_int[1] < 1) || (xr_int[2] > d[2])
  out_y <- (yr_int[1] < 1) || (yr_int[2] > d[1])
  
  if( out_x || out_y ){
    
    xr_orig <- range(coords$x, na.rm = TRUE)
    yr_orig <- range(coords$y, na.rm = TRUE)
    xr_orig <- c(floor(xr_orig[1]), ceiling(xr_orig[2]))
    yr_orig <- c(floor(yr_orig[1]), ceiling(yr_orig[2]))
    
    msg <- "Graph coordinates outside the image boundaries."
    
    ms_i <- c("i" = "Note: node coordinates are treated as indices of the image matrix.")
    
    ms_x1 <- c(">" = sprintf("Node coordinate ranges: x[%s, %s], y[%s, %s].", 
      xr_orig[1], xr_orig[2], yr_orig[1], yr_orig[2]))
      
    ms_x2 <- c(">" = sprintf("Image dimensions: %s x %s (rows x cols).", d[1], d[2]))
    
    ms_a1 <- c("*" = "Try adjusting 'flip' and 'rotate' options in `normalizeGraphSpace()`.")
    ms_a2 <- c("*" = "Alternatively, set `image.space = FALSE` if normalization should not map to image indices.")
    
    ms_f <- "See `vignette('RGraphSpace')` for more information on coordinate normalization."
    
    rlang::abort(message = msg, 
      body = c(ms_i, ms_x1, ms_x2, ms_a1, ms_a2), footer = ms_f, 
      call = rlang::caller_env())
    
  }
  
  invisible(TRUE)
  
}

################################################################################
### Adjust image to node coordinates
################################################################################

#-------------------------------------------------------------------------------
.fitImageNodes <- function(nodes, image, mar = 0.1){
  
  l_temp <- .fit_image_nodes(nodes, image, mar)
  l_temp <- .adjust_aspect_ratio(l_temp)
  l_temp <- .normalize_image_nodes(l_temp)
  
  return(l_temp)
}

#-------------------------------------------------------------------------------
# Fit image to nodes with focus on adjusting graph margins
.fit_image_nodes <- function(nodes, image, mar = 0.1) {
  
  nds <- nodes
  img <- image
  d <- dim(img)
  mar <- max(0, min(mar, 0.49))
  
  # bounding box around nodes
  xl_nds <- range(nds$x)
  yl_nds <- range(nds$y)
  center_x <- mean(xl_nds)
  center_y <- mean(yl_nds)
  
  # target dimension centered on nodes;
  # side_length is calculated so that max_d is 
  # exactly (1 - 2*mar) of the total width
  max_d <- max(diff(xl_nds), diff(yl_nds))
  side_length <- max_d / (1 - 2 * mar)
  half_side <- side_length / 2
  
  # initial crop coordinates
  x_start <- center_x - half_side
  x_end   <- x_start + side_length
  y_start <- center_y - half_side
  y_end   <- y_start + side_length
  
  # shift crop coordinates to the image boundaries
  if (x_start < 1) { 
    shift <- 1 - x_start
    x_start <- 1
    x_end <- min(d[2], x_end + shift)
  }
  if (x_end > d[2]) { 
    shift <- x_end - d[2]
    x_end <- d[2]
    x_start <- max(1, x_start - shift)
  }
  if (y_start < 1) { 
    shift <- 1 - y_start
    y_start <- 1
    y_end <- min(d[1], y_end + shift)
  }
  if (y_end > d[1]) { 
    shift <- y_end - d[1]
    y_end <- d[1]
    y_start <- max(1, y_start - shift)
  }
  
  # force the limits to include the node bounding box
  x_start <- max(1, min(x_start, xl_nds[1]))
  x_end   <- min(d[2], max(x_end, xl_nds[2]))
  y_start <- max(1, min(y_start, yl_nds[1]))
  y_end   <- min(d[1], max(y_end, yl_nds[2]))
  
  # convert to indices
  x_s_idx <- floor(x_start)
  x_e_idx   <- ceiling(x_end)
  y_s_idx <- floor(y_start)
  y_e_idx   <- ceiling(y_end)
  
  # final validity check
  x_s_idx <- max(1L, x_s_idx)
  x_e_idx   <- min(d[2], x_e_idx)
  y_s_idx <- max(1L, y_s_idx)
  y_e_idx   <- min(d[1], y_e_idx)
  
  # execute crop on flipped image
  img_res <- img[seq.int(d[1], 1), ]
  img_res <- img_res[seq.int(y_s_idx, y_e_idx), seq.int(x_s_idx, x_e_idx)]
  img_res <- img_res[seq.int(nrow(img_res), 1), ]
  
  # update node coordinates
  nds$x <- nds$x - x_s_idx + 1
  nds$y <- nds$y - y_s_idx + 1
  
  # calculate final side_length
  x_side_length <- x_end - x_start
  y_side_length <- y_end - y_start
  x_side_length <- x_side_length + (x_start - x_s_idx)
  y_side_length <- y_side_length + (y_start - y_s_idx)
  side_length <- max(x_side_length, y_side_length)
  
  return(list(nodes = nds, image = img_res, 
    side_length = side_length))
}

#-------------------------------------------------------------------------------
.adjust_aspect_ratio <- function(l_temp){
  d <- dim(l_temp$image)
  if(d[1] > d[2]){
    n <- ceiling( (d[1] - d[2]) )/2
    img_d <- matrix(NA, nrow = d[1], ncol = d[1])
    img_d[ , seq(n + 1, n + d[2])] <- as.matrix(l_temp$image)
    l_temp$nodes$x <- l_temp$nodes$x + n
    l_temp$image  <- as.raster(img_d)
  } else if(d[1] < d[2]){
    n <- ceiling( (d[2] - d[1])/2 )
    img_d <- matrix(NA, nrow = d[2], ncol = d[2])
    img_d[seq(n + 1, n + d[1]), ] <- as.matrix(l_temp$image)
    l_temp$nodes$y <- l_temp$nodes$y + n
    l_temp$image  <- as.raster(img_d)
  }
  return(l_temp)
}

#-------------------------------------------------------------------------------
.normalize_image_nodes <- function(l_temp){
  d <- dim(l_temp$image)
  l_temp$nodes$x <- .rescale_direct(l_temp$nodes$x, d[2], 0.5 / d[2])
  l_temp$nodes$y <- .rescale_direct(l_temp$nodes$y, d[1], 0.5 / d[1])
  return(l_temp)
}
.rescale_direct <- function(x, n, half_pixel) {
  ((x - 1) / (n - 1)) * (1 - 2 * half_pixel) + half_pixel
}

################################################################################
### Crop graph and image
################################################################################

#-------------------------------------------------------------------------------
.crop_gspace <- function(gs, crop.coord) {
  if (gs@pars$image.layer) {
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
  
  # Filter nodes within the crop window
  nodes <- gs@nodes
  cx <- nodes$x >= xmin & nodes$x <= xmax
  cy <- nodes$y >= ymin & nodes$y <= ymax
  nodes <- nodes[which(cx & cy), ]
  
  gs <- .crop_update_graph(gs, nodes)
  
  return(gs)
  
}

#-------------------------------------------------------------------------------
.crop_gspace_image <- function(gs, crop.coord) {
  
  xmin <- crop.coord[1]; xmax <- crop.coord[2]
  ymin <- crop.coord[3]; ymax <- crop.coord[4]
  
  # Filter nodes within the crop window
  nodes <- gs@nodes
  
  # Compute image crop indices
  nrow_mat <- nrow(gs@image)
  ncol_mat <- ncol(gs@image)
  col_s <- max(1L, ceiling(xmin * ncol_mat))
  col_e <- min(ncol_mat, floor(xmax * ncol_mat))
  row_s <- max(1L, ceiling((1 - ymax) * nrow_mat))
  row_e <- min(nrow_mat, floor((1 - ymin) * nrow_mat))
  d <- c(row_e - row_s + 1L, col_e - col_s + 1L)
  
  # Reverse pixel-center encoding to recover 1-based pixel indices
  # x: shift to crop-window origin (left edge)
  nodes$x <- .rescale_direct_inv(nodes$x, ncol_mat, 0.5 / ncol_mat) - (col_s - 1)
  # y: shift to crop-window origin (top edge, image convention)
  nodes$y <- .rescale_direct_inv(nodes$y, nrow_mat, 0.5 / nrow_mat) - (nrow_mat - row_e)
  
  # Re-encode to pixel centers in the cropped image
  nodes$x <- .rescale_direct(nodes$x, d[2], 0.5 / d[2])
  nodes$y <- .rescale_direct(nodes$y, d[1], 0.5 / d[1])
  
  # Crop nodes
  cx <- nodes$x >= 0 & nodes$x <= 1
  cy <- nodes$y >= 0 & nodes$y <= 1
  nodes <- nodes[which(cx & cy), ]
  
  # Crop image
  gs@image <- gs@image[row_s:row_e, col_s:col_e, drop = FALSE]
  
  gs <- .crop_update_graph(gs, nodes)
  
  return(gs)
  
}

#-------------------------------------------------------------------------------
.rescale_direct_inv <- function(x, n, half_pixel) {
  ((x - half_pixel) / (1 - 2 * half_pixel)) * (n - 1) + 1
}

#-------------------------------------------------------------------------------
.crop_update_graph <- function(gs, nodes) {
  
  # Remove edges whose endpoints are no longer in the node set
  idx <- (gs@edges$name1 %in% nodes$name) &
    (gs@edges$name2 %in% nodes$name)
  gs@edges <- gs@edges[idx, ]
  
  # Update graph vertices
  idx <- V(gs@graph)$name %in% rownames(nodes)
  gs@graph <- igraph::delete_vertices(gs@graph, which(!idx))
  idx <- match(rownames(nodes), V(gs@graph)$name)
  V(gs@graph)$x[idx] <- nodes$x
  V(gs@graph)$y[idx] <- nodes$y
  gs@nodes <- nodes
  
  return(gs)
  
}



