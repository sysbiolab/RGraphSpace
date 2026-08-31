
#-------------------------------------------------------------------------------
#' Normalize node coordinates to graph and image spaces
#' 
#' Accessory function to normalize node coordinates of a \link{GraphSpace} 
#' object, either by centering nodes within the plot boundaries or by mapping 
#' nodes to pixel coordinates of a background image.
#' 
#' @param gs A \code{GraphSpace} object to be normalized.
#' @param mar A single numeric value in \code{[0, 0.5]} setting the margins
#' around the graph, as a fraction of the final normalized space. For example,
#' \code{mar = 0.1} leaves a margin of 0.1 on each side, so the graph occupies
#' the central 0.8 of the space. With an image, the image is cropped to the same
#' proportions; if the graph lies close to an image border, the crop is shifted
#' or truncated to stay within the image, and the requested margin may not be
#' reached.
#' @param image.space Logical; if an image is available, whether to use it as 
#' a background reference map. When enabled, \code{x} and \code{y} graph 
#' coordinates are interpreted as pixel coordinates in the image matrix. 
#' Images can be inspected and assigned with \code{\link{gs_image}}.
#' @param flip.y Logical; whether to flip the node coordinates along the y-axis.
#' Useful for aligning nodes with image backgrounds, which often use an 
#' inverted coordinate system. Defaults to \code{image.space}.
#' @param flip.x Logical; whether to flip the node coordinates along the x-axis.
#' @param flip.v Logical; whether to vertically flip the background image  
#' matrix (top-to-bottom) to align with the graph coordinate system.
#' @param flip.h Logical; whether to horizontally flip the background image  
#' matrix (left-to-right) to align with the graph coordinate system.
#' @param swap.xy Logical; whether to swap x and y node coordinates. 
#' Useful when the graph coordinate system is transposed relative to the
#' image or reference map.
#' @param equal.mar Logical; when an image is available, whether to fit the image
#' with equal margins around the graph, resulting in a tighter crop of the image.
#' If FALSE (default), the image is fitted to the full square figure area,
#' resulting in unequal margins when the graph aspect ratio differs from 1. Both
#' methods preserve the aspect ratios of the image and graph.
#' @param norm.geometry Logical; when geometries are available, whether to
#' normalize them. If `TRUE`, \link{normalizeGeometry} is called at the end
#' of the normalization process.
#' @param verbose A single logical value specifying to display detailed 
#' messages (when \code{verbose=TRUE}) or not (when \code{verbose=FALSE}).
#' 
#' @details 
#' This function re-scales node coordinates to a \code{[0, 1]} unit square 
#' based on the graph's bounding box when \code{image.space = FALSE} or, when
#' an image is provided and \code{image.space = TRUE}, it maps nodes to pixel 
#' coordinates. It handles image-to-graph alignment via \code{flip.\*} and 
#' \code{swap.\*} arguments, used to adjust the graph origin with the image 
#' matrix layout. Users should be aware of the potential discrepancy between 
#' image matrix orientation (top-down) and graph coordinates (bottom-up). The 
#' function attempts to automatically adjust the y-axis to align the graph's 
#' bottom-up coordinates with the image's top-down layout, but further manual 
#' adjustments might be required.
#' 
#' @return A \code{GraphSpace} object with updated \code{nodes} 
#' and \code{image} slots.
#' 
#' @note This is an accessory function typically called during 
#' the preprocessing of \code{GraphSpace} objects before rendering.
#' 
#' @seealso \code{\link{cropGraphSpace}}, \code{\link{gs_image}}
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
#' plotGraphSpace(gs, add.labels = TRUE)
#' 
#' @aliases normalizeGraphSpace
#' @rdname normalizeGraphSpace-methods
#' @export
setMethod("normalizeGraphSpace", "GraphSpace",
  function(gs, mar = 0.1, image.space = .has_image(gs), 
    flip.x = FALSE, flip.y = image.space,  
    flip.v = FALSE, flip.h = FALSE, 
    swap.xy = FALSE, equal.mar = FALSE, 
    norm.geometry = FALSE, verbose = TRUE){
    
    gs <- updateGraphSpace(gs)
    
    .validate_gs_args("singleLogical", "image.space", image.space)
    .validate_gs_args("singleNumber", "mar", mar)
    .validate_gs_args("singleLogical", "flip.v", flip.v)
    .validate_gs_args("singleLogical", "flip.h", flip.h)
    .validate_gs_args("singleLogical", "flip.x", flip.x)
    .validate_gs_args("singleLogical", "flip.y", flip.y)
    .validate_gs_args("singleLogical", "swap.xy", swap.xy)
    .validate_gs_args("singleLogical", "equal.mar", equal.mar)
    .validate_gs_args("singleLogical", "norm.geometry", norm.geometry)
    .validate_gs_args("singleLogical", "verbose", verbose)
    
    if (mar < 0 || mar > 0.5) {
      rlang::warn("'mar' should be in [0, 0.5]")
      mar <- max(0, min(mar, 0.5))
    }
    if(image.space && !.has_image(gs)){
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
          swap.xy, flip.v, flip.h, equal.mar, verbose)
      } else {
        gs <- .normalizeGraphSpace.graph(gs, mar, flip.x, flip.y, 
          swap.xy, verbose)
      }
    }
    
    if(norm.geometry){
      valid_names <- .gs_geometry_cols(getGraphSpace(gs, "coords"))
      if(length(valid_names)>0){
        for(name in valid_names){
          gs <- normalizeGeometry(gs, name = name, verbose = verbose)
        }
      }
    }
    
    return(gs)
    
  }
)

#-------------------------------------------------------------------------------
#' @importFrom sf st_coordinates st_centroid st_set_crs
#' @importFrom stats lm residuals coef
#' @keywords internal
.normalizeGraphSpace.graph <- function(gs, mar, flip.x, flip.y, 
  swap.xy, verbose){
  
  if(verbose) rlang::inform("Normalizing node coordinates to graph space...")
  nodes <- .set_raw_coords(gs@nodes, gs@coords)
  nodes <- .setCoordToGraph(nodes, flip.x, flip.y, swap.xy, verbose)
  gs@nodes <- .fit_graph_space(nodes, mar)
  gs@pars$image.space <- FALSE
  gs@pars$is.normalized <- TRUE
  gs@pars$flip.x <- flip.x
  gs@pars$flip.y <- flip.y
  gs@pars$swap.xy <- swap.xy
  gs@pars$mar <- mar
  
  return(gs)
  
}

#-------------------------------------------------------------------------------
.normalizeGraphSpace.image <- function(gs, mar, flip.x, flip.y, 
  swap.xy, flip.v, flip.h, equal.mar, verbose){
  
  if(verbose) rlang::inform("Normalizing node coordinates to image space...")
  
  nodes <- .set_raw_coords(gs@nodes, gs@coords)
  image <- .get_image(gs)
  maxpixels <- gs_image_maxpixels(gs)
  
  if(flip.v){
    if(verbose) rlang::inform("Flipping image top-to-bottom...")  
    image <- .flip_image(image, vertical = TRUE)
  } 
  
  if(flip.h){
    if(verbose) rlang::inform("Flipping image left-to-right...")  
    image <- .flip_image(image, vertical = FALSE)
  } 
  
  nodes <- .setCoordToImage(nodes, image, flip.x, flip.y, swap.xy, verbose)
  l_temp <- .fitImageNodes(nodes, image, mar, maxpixels, equal.mar)
  gs@nodes <- l_temp$nodes
  gs@canvas <- l_temp$image
  gs@pars$is.normalized <- TRUE
  gs@pars$image.space <- TRUE
  gs@pars$flip.v <- flip.v
  gs@pars$flip.x <- flip.x
  gs@pars$flip.y <- flip.y
  gs@pars$swap.xy <- swap.xy
  gs@pars$mar <- mar
  
  return(gs)
}

################################################################################
### Graph adjusts
################################################################################
.setCoordToGraph <- function(nodes, flip.x = FALSE, flip.y = FALSE, 
  swap.xy = FALSE, verbose = TRUE){
  
  # swap coordinates
  coord_xy <- nodes[,c("x","y")]
  if(swap.xy){
    if(verbose) rlang::inform("Swapping xy-coordinates...")
    coord_xy$x2 <- coord_xy$y
    coord_xy$y2 <- coord_xy$x
  } else {
    coord_xy$x2 <- coord_xy$x
    coord_xy$y2 <- coord_xy$y
  }
  
  # Flip y-coordinates
  if(flip.y){
    if(verbose) rlang::inform("Flipping y-coordinates over graph center...")
    y <- coord_xy$y2
    coord_xy$y2 <- max(y) + min(y) - y
  }
  
  # Flip x-coordinates
  if(flip.x){
    if(verbose) rlang::inform("Flipping x-coordinates over graph center...")
    x <- coord_xy$x2
    coord_xy$x2 <- max(x) + min(x) - x
  }
  # Update coordinates
  nodes$x <- coord_xy$x2
  nodes$y <- coord_xy$y2
  
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
    
    if(diff(from) == 0){
      nds$x <- 0.5
      nds$y <- 0.5
    } else {
      nds$x <- scales::rescale(nds$x, from = from, to=to)
      nds$y <- scales::rescale(nds$y, from = from, to=to)
    }
    
  }
  
  return(nds)
}

################################################################################
### Graph-to-image adjusts
################################################################################
.setCoordToImage <- function(nodes, image, 
  flip.x = FALSE, flip.y = FALSE, swap.xy = FALSE, 
  verbose = TRUE){
  
  # swap coordinates
  coord_xy <- nodes[,c("x","y")]
  if(swap.xy){
    if(verbose) rlang::inform("Swapping xy-coordinates...")
    coord_xy$x2 <- coord_xy$y
    coord_xy$y2 <- coord_xy$x
  } else {
    coord_xy$x2 <- coord_xy$x
    coord_xy$y2 <- coord_xy$y
  }
  
  if(flip.y){
    if(verbose) rlang::inform("Flipping y-coordinates over image center...")
    y <- coord_xy$y2
    y <- -(y - max(y)) + nrow(image) - max(y) + 1
    coord_xy$y2 <- y
  }
  
  if(flip.x){
    if(verbose) rlang::inform("Flipping x-coordinates over image center...")
    x <- coord_xy$x2
    x <- -(x - max(x)) + ncol(image) - max(x) + 1
    coord_xy$x2 <- x
  }
  
  # Update coordinates
  .check_final_coords(coord_xy, image)
  
  nodes$x <- coord_xy$x2
  nodes$y <- coord_xy$y2
  
  return(nodes)
  
}

#-------------------------------------------------------------------------------
.check_final_coords <- function(coord_xy, image){
  
  d <- dim(image)
  xr <- range(coord_xy$x2, na.rm = TRUE)
  yr <- range(coord_xy$y2, na.rm = TRUE)
  
  xr_int <- c(floor(xr[1]), ceiling(xr[2]))
  yr_int <- c(floor(yr[1]), ceiling(yr[2]))
  
  out_x <- (xr_int[1] < 1) || (xr_int[2] > d[2])
  out_y <- (yr_int[1] < 1) || (yr_int[2] > d[1])
  
  if( out_x || out_y ){
    
    xr_orig <- range(coord_xy$x, na.rm = TRUE)
    yr_orig <- range(coord_xy$y, na.rm = TRUE)
    xr_orig <- c(floor(xr_orig[1]), ceiling(xr_orig[2]))
    yr_orig <- c(floor(yr_orig[1]), ceiling(yr_orig[2]))
    
    msg <- "Graph coordinates fall outside the image boundaries."
    
    ms_i <- c("i" = "Note: node coordinates are mapped as indices of the image matrix.")
    
    ms_x1 <- c(">" = sprintf("Node ranges: x[%s, %s] (cols), y[%s, %s] (rows).", 
      xr_orig[1], xr_orig[2], yr_orig[1], yr_orig[2]))
      
    ms_x2 <- c(">" = sprintf("Image dimensions: %s cols x %s rows.", d[2], d[1]))
    
    ms_a1 <- c("*" = "Try adjusting 'flip' and 'swap' in `normalizeGraphSpace()`.")
    ms_a2 <- c("*" = "Or set `image.space = FALSE` to skip image-index mapping.")
    
    footer = c(
      "For details on coordinate normalization, visit the online tutorial:",
      "https://sysbiolab.github.io/RGraphSpace/"
    )
    
    rlang::abort(message = msg, 
      body = c(ms_i, ms_x1, ms_x2, ms_a1, ms_a2), 
      footer = footer, 
      call = rlang::caller_env())
    
  }
  
  invisible(TRUE)
  
}

################################################################################
### Adjust image to node coordinates
################################################################################

#-------------------------------------------------------------------------------
.fitImageNodes <- function(nodes, image, mar, maxpixels, equal.mar){
  
  # gs_image() (the getter) tags its return value with class "gs_image" for
  # downstream handler-recognition purposes (see .is_handler()); the @image
  # slot itself only ever stores plain "raster" (its setter enforces this).
  # only enforce raster class for genuine rasters; leave SpatRaster untouched
  if (!inherits(image, "SpatRaster")) {
    class(image) <- "raster"
  }
  
  # Degenerate case: all nodes share the same (x, y) -- mirrors the explicit
  # guard in .fit_graph_space() for the non-image path. Without this, the
  # crop window collapses to a 1-pixel image and .normalize_image_nodes()
  # divides by (n - 1) = 0. Handled here, at the point where "center the
  # point, use the image as-is".
  if ( diff(range(nodes$x)) == 0 && diff(range(nodes$y)) == 0 ) {
    nodes$x <- 0.5
    nodes$y <- 0.5
    return(list(nodes = nodes, image = image, side_length = NA))
  }
  
  l_temp <- .fit_image_nodes(nodes, image, mar, maxpixels, equal.mar)
  l_temp <- .adjust_aspect_ratio(l_temp)
  l_temp <- .normalize_image_nodes(l_temp)
  
  return(l_temp)
}

#-------------------------------------------------------------------------------
# Fit image to nodes with focus on adjusting graph margins
.fit_image_nodes <- function(nodes, image, mar, maxpixels, equal.mar = FALSE) {
  
  d <- dim(image)
  mar <- max(0, min(mar, 0.49))
  
  # bounding box around nodes
  xl_nds <- range(nodes$x)
  yl_nds <- range(nodes$y)
  center_x <- mean(xl_nds)
  center_y <- mean(yl_nds)
  
  # set initial crop coordinates
  if(equal.mar){
    side_length_x <- diff(xl_nds) / (1 - 2 * mar)
    side_length_y <- diff(yl_nds) / (1 - 2 * mar)
    x_start <- center_x - side_length_x/2
    x_end   <- x_start + side_length_x
    y_start <- center_y - side_length_y/2
    y_end   <- y_start + side_length_y
  } else {
    max_d <- max(diff(xl_nds), diff(yl_nds))
    side_length <- max_d / (1 - 2 * mar)
    half_side <- side_length / 2
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
  }
  
  # force the limits to include the node bounding box
  x_start <- max(1, min(x_start, xl_nds[1]))
  x_end <- min(d[2], max(x_end, xl_nds[2]))
  y_start <- max(1, min(y_start, yl_nds[1]))
  y_end <- min(d[1], max(y_end, yl_nds[2]))
  
  # convert to indices
  x_s_idx <- floor(x_start)
  x_e_idx <- ceiling(x_end)
  y_s_idx <- floor(y_start)
  y_e_idx <- ceiling(y_end)
  
  # final validity check
  x_s <- max(1L, x_s_idx)
  x_e <- min(d[2], x_e_idx)
  y_s <- max(1L, y_s_idx)
  y_e <- min(d[1], y_e_idx)
  
  # execute crop on the node window 
  # raster: matrix subset; SpatRaster: lazy read
  img_res <- .crop_image_window(image, y_s, y_e, x_s, x_e,
    maxpixels = maxpixels)
  
  # update node coordinates
  nodes$x <- nodes$x - x_s + 1
  nodes$y <- nodes$y - y_s + 1
  
  return(list(nodes = nodes, image = img_res))
}

#-------------------------------------------------------------------------------
# Crop the node-bounding window from the image.
#   raster:     flip, matrix-subset the window, flip back (original behavior)
#   SpatRaster: read only that window via terra (lazy), downsampled so the
#               result is about `maxpixels` pixels -- full detail on small
#               windows, coarse on large ones, without materializing the source.
.crop_image_window <- function(img, y_s, y_e, x_s, x_e, maxpixels = 4e6) {
  
  if (inherits(img, "SpatRaster")) {
    
    # work in pixel-index space: make the raster's extent match its pixel dims
    nr <- nrow(img); nc <- ncol(img)
    terra::ext(img) <- c(0, nc, 0, nr)
    
    # crop on flipped image
    img <- terra::flip(img, direction = "vertical")   # flip
    ext <- terra::ext(x_s - 1, x_e, nr - y_e, nr - (y_s - 1))
    win <- terra::crop(img, ext)
    win <- terra::flip(win, direction = "vertical")   # flip back
    terra::ext(win) <- c(0, ncol(win), 0, nrow(win))
    orig_dim <- dim(win)
    
    # downsample the window to about maxpixels (aggregate by an integer factor)
    npix <- terra::ncell(win)
    if (npix > maxpixels) {
      scale  <- sqrt(maxpixels / npix)
      new_nc <- max(1L, floor(ncol(win) * scale))
      new_nr <- max(1L, floor(nrow(win) * scale))
      target <- terra::rast(nrows = new_nr, ncols = new_nc,
        extent = terra::ext(win), crs = terra::crs(win))
      win <- terra::resample(win, target, method = "bilinear")
    }
    # img_res <- .spatraster_to_raster(win)
    img_res <- win
    attr(img_res, "orig_dim") <- orig_dim
    return(img_res)
  }
  
  # raster path -- original behavior
  # crop on flipped image
  img_res <- img[seq.int(nrow(img), 1), ]
  img_res <- img_res[seq.int(y_s, y_e), seq.int(x_s, x_e)]
  img_res <- img_res[seq.int(nrow(img_res), 1), ]
  attr(img_res, "orig_dim") <- dim(img_res)
  return(img_res)
  
}

#-------------------------------------------------------------------------------
# adjust aspect ratio at pixel-space nodes
.adjust_aspect_ratio <- function(l_temp){
  od <- attr(l_temp$image, "orig_dim")
  p  <- .pad_image_square(l_temp$image)
  l_temp$image <- p$image
  if (!is.na(p$axis)) {
    if (p$axis == "x") {
      s <- od[1] / p$d[1] # full-res / downsampled (rows)
      l_temp$nodes$x <- l_temp$nodes$x + p$n * s
      od <- c(od[1], round(od[2]*p$d[1]/p$d[2])) 
    } else {
      s <- od[2] / p$d[2] # full-res / downsampled (cols)
      l_temp$nodes$y <- l_temp$nodes$y + (p$d[2] - p$d[1] - p$n) * s
      od <- c(round(od[1]*p$d[2]/p$d[1]), od[2]) 
    }
  }
  attr(l_temp$image, "orig_dim") <- od
  return(l_temp)
}

#-------------------------------------------------------------------------------
# shared: pad an image (raster or SpatRaster) to square with NA fill.
# returns list(image = padded, n = pad, axis = "x"|"y") so the caller
# can apply its own node adjustment.
.pad_image_square <- function(img){
  d <- dim(img)
  if(d[1] > d[2]){
    n <- ceiling((d[1] - d[2])/2)
    if (inherits(img, "SpatRaster")){
      terra::ext(img) <- c(0, d[2], 0, d[1])
      img <- terra::extend(img, terra::ext(-n, d[2] + (d[1]-d[2]-n), 0, d[1]))
    } else {
      img_d <- matrix(NA, nrow = d[1], ncol = d[1])
      img_d[ , seq(n + 1, n + d[2])] <- as.matrix(img)
      img <- as.raster(img_d)
    }
    return(list(image = img, n = n, axis = "x", d = d))
  } else if(d[1] < d[2]){
    n <- ceiling((d[2] - d[1])/2)
    if (inherits(img, "SpatRaster")){
      terra::ext(img) <- c(0, d[2], 0, d[1])
      # img <- terra::extend(img, terra::ext(0, d[2], -n, d[1] + (d[2]-d[1]-n)))
      img <- terra::extend(img, terra::ext(0, d[2], -(d[2]-d[1]-n), d[1] + n))
    } else {
      img_d <- matrix(NA, nrow = d[2], ncol = d[2])
      img_d[seq(n + 1, n + d[1]), ] <- as.matrix(img)
      img <- as.raster(img_d)
    }
    return(list(image = img, n = n, axis = "y", d = d))
  }
  list(image = img, n = 0, axis = NA, d = d)
}

#-------------------------------------------------------------------------------
.normalize_image_nodes <- function(l_temp){
  d <- attr(l_temp$image, "orig_dim")
  l_temp$nodes$x <- .rescale_direct(l_temp$nodes$x, d[2], 0.5 / d[2])
  l_temp$nodes$y <- .rescale_direct(l_temp$nodes$y, d[1], 0.5 / d[1])
  return(l_temp)
}

#-------------------------------------------------------------------------------
.rescale_direct <- function(x, n, half_pixel) {
  ((x - 1) / (n - 1)) * (1 - 2 * half_pixel) + half_pixel
}

#-------------------------------------------------------------------------------
.rescale_direct_inv <- function(x, n, half_pixel) {
  ((x - half_pixel) / (1 - 2 * half_pixel)) * (n - 1) + 1
}

#-------------------------------------------------------------------------------
.denormalize_graph_space <- function(x, verbose = TRUE) {
  if (verbose) rlang::inform("Denormalizing graph coordinates...")
  x@pars$is.normalized <- FALSE
  x@pars$image.space   <- FALSE
  x@canvas <- as.raster(matrix())
  x@nodes <- .set_raw_coords(x@nodes, x@coords)
  for (col in .gs_geometry_cols(x@coords)) {
    x@nodes[[col]] <- x@coords[[col]]
  }
  return(x)
}

#-------------------------------------------------------------------------------
.set_raw_coords <- function(nodes, coords) {
  nodes[rownames(coords), c("x","y")] <- coords[, c("x","y")]
  nodes
}

