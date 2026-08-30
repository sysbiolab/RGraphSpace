
#-------------------------------------------------------------------------------
#' Crop, rotate, flip, and transpose a GraphSpace
#' 
#' Accessory functions to spatially transform a normalized
#' \link{GraphSpace} object. \code{cropGraphSpace()} subsets the plotting
#' area to a rectangular region; \code{rotateGraphSpace()} rotates by a
#' quarter turn; \code{flipGraphSpace()} mirrors horizontally or vertically;
#' \code{transposeGraphSpace()} swaps the x and y axes.
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
#' @param clockwise Logical; if \code{FALSE} (default), the
#' 90-degree turn is counter-clockwise; if \code{TRUE}, clockwise
#' (\code{rotateGraphSpace} only).
#' @param vertical Logical; if \code{FALSE} (default), the flip is horizontal 
#' (mirror left-right); if \code{TRUE}, vertical (mirror top-bottom).
#' (\code{flipGraphSpace} only).
#' @param persist Logical; whether the transformation persists through
#' re-normalization. Defaults to \code{TRUE} before normalization,
#' \code{FALSE} after.
#' @param verbose A single logical value specifying to display detailed
#' messages (when \code{verbose=TRUE}) or not (when \code{verbose=FALSE}).
#' 
#' @details 
#' \code{cropGraphSpace()} subsets a normalized graph space to a specific
#' region defined by the cropping boundaries. It recalculates node positions
#' and background image boundaries to maintain spatial consistency after
#' cropping, and drops nodes (and edges) that fall outside the window.
#' 
#' \code{rotateGraphSpace()}, \code{flipGraphSpace()}, and
#' \code{transposeGraphSpace()} are all exact, a coordinate/pixel
#' permutation, with no resampling, no interpolation, and no risk of
#' misaligning nodes against the background image.
#' \code{rotateGraphSpace()} is restricted to a single 90-degree turn: apply 
#' it again to its own output for 180 or 270 degrees, e.g.
#' \code{rotateGraphSpace(rotateGraphSpace(gs))} for 180 degrees. Combine
#' all three with each other to reach any of the 8 symmetries of a square.
#' 
#' @return A \code{GraphSpace} object with updated \code{nodes} 
#' and \code{canvas} slots.
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
#' gs_rot90 <- rotateGraphSpace(gs)
#' gs_flip <- flipGraphSpace(gs)
#' gs_t <- transposeGraphSpace(gs)
#' 
#' plotGraphSpace(gs, add.labels = TRUE)
#' 
#' plotGraphSpace(gs_crop, add.labels = TRUE)
#' 
#' @aliases cropGraphSpace
#' @rdname GraphSpace-transform
#' @export
setMethod("cropGraphSpace", "GraphSpace", 
  function(gs, xmin = 0, xmax = 1, ymin = 0, ymax = 1, verbose = TRUE){
    
    gs <- updateGraphSpace(gs)
    
    if(!.is_normalized(gs)){
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
    .validate_gs_args("singleLogical", "verbose", verbose)
    
    if(xmin >= xmax || ymin >= ymax){
      rlang::abort("'crop' must satisfy xmin < xmax and ymin < ymax.")
    }
    
    if(verbose){
      rlang::inform(sprintf(
        "Cropping graph space to x in [%s, %s], y in [%s, %s]...",
        xmin, xmax, ymin, ymax))
    }
    
    gs <- .crop_gspace(gs, crop.box = c(xmin, xmax, ymin, ymax))
    
    return(gs)
    
  })

#' @aliases flipGraphSpace
#' @rdname GraphSpace-transform
#' @export
setMethod("flipGraphSpace", "GraphSpace",
  function(gs, vertical = FALSE, persist = .is_raw(gs), verbose = TRUE){
    
    gs <- updateGraphSpace(gs)
    
    .validate_gs_args("singleLogical", "vertical", vertical)
    .validate_gs_args("singleLogical", "persist", persist)
    .validate_gs_args("singleLogical", "verbose", verbose)
    
    if (persist) {
      if(verbose){
        rlang::inform(sprintf("Flipping raw coordinates %s...",
          if(vertical) "vertically" else "horizontally"))
      }
      gs <- .flip_gspace_raw(gs, vertical)
    } else {
      if(verbose){
        rlang::inform(sprintf("Flipping normalized coordinates %s...",
          if(vertical) "vertically" else "horizontally"))
      }
      gs <- .flip_gspace(gs, vertical)
    }
    
    return(gs)
    
  })

#' @aliases rotateGraphSpace
#' @rdname GraphSpace-transform
#' @export
setMethod("rotateGraphSpace", "GraphSpace",
  function(gs, clockwise = FALSE, persist = .is_raw(gs), verbose = TRUE){
    
    gs <- updateGraphSpace(gs)
    
    .validate_gs_args("singleLogical", "clockwise", clockwise)
    .validate_gs_args("singleLogical", "verbose", verbose)
    
    quarter_turns <- if(clockwise) 3L else 1L
    
    if (persist) {
      if(verbose){
        rlang::inform(sprintf("Rotating raw coordinates 90 degrees %s...",
          if(clockwise) "clockwise" else "counter-clockwise"))
      }
      gs <- .rotate_gspace_raw(gs, quarter_turns)
    } else {
      if(verbose){
        rlang::inform(sprintf("Rotating normalized coordinates 90 degrees %s...",
          if(clockwise) "clockwise" else "counter-clockwise"))
      }
      gs <- .rotate_gspace(gs, quarter_turns)
    }
    
    return(gs)
    
  })

#' @aliases transposeGraphSpace
#' @rdname GraphSpace-transform
#' @export
setMethod("transposeGraphSpace", "GraphSpace",
  function(gs, persist = .is_raw(gs), verbose = TRUE){
    
    gs <- updateGraphSpace(gs)
    
    .validate_gs_args("singleLogical", "persist", persist)
    .validate_gs_args("singleLogical", "verbose", verbose)
    
    if ( persist ) {
      if (verbose) rlang::inform("Transposing raw coordinates...")
      gs <- .transpose_gspace_raw(gs)
    } else {
      if (verbose) rlang::inform("Transposing normalized coordinates...")
      gs <- .transpose_gspace(gs)
    }
    
    return(gs)
    
  })

################################################################################
### Crop graph and image
################################################################################

#-------------------------------------------------------------------------------
.crop_gspace <- function(gs, crop.box) {
  if (.has_image(gs)) {
    gs <- .crop_gspace_image(gs, crop.box)
  } else {
    gs <- .crop_gspace_graph(gs, crop.box)
  }
  return(gs)
}

#-------------------------------------------------------------------------------
.crop_gspace_graph <- function(gs, crop.box) {
  
  xmin <- crop.box[1]; xmax <- crop.box[2]
  ymin <- crop.box[3]; ymax <- crop.box[4]
  
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
.crop_gspace_image <- function(gs, crop.box) {
  
  xmin <- crop.box[1]; xmax <- crop.box[2]
  ymin <- crop.box[3]; ymax <- crop.box[4]
  
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
  if (inherits(canvas, "SpatRaster")) {
    nr <- nrow(canvas); nc <- ncol(canvas)
    terra::ext(canvas) <- c(0, nc, 0, nr)
    ext <- terra::ext(col_s - 1, col_e, nr - row_e, nr - (row_s - 1))
    gs@canvas <- terra::crop(canvas, ext)
  } else {
    gs@canvas <- canvas[row_s:row_e, col_s:col_e, drop = FALSE]
  }
  gs <- .trim_graph_space(gs, nodes)
  gs <- .adjust_box_canvas(gs)
  return(gs)
  
}

#-------------------------------------------------------------------------------
# adjust box canvas at normalized-space nodes
.adjust_box_canvas <- function(gs){
  p <- .pad_image_square(gs@canvas)
  gs@canvas <- p$image
  if (!is.na(p$axis)) {
    s <- p$n / (if (p$axis == "x") p$d[1] else p$d[2])
    if (p$axis == "x"){
      gs@nodes$x <- scales::rescale(gs@nodes$x, from = c(0,1), to = c(s, 1-s))
    } else {
      gs@nodes$y <- scales::rescale(gs@nodes$y, from = c(0,1), to = c(s, 1-s))
    }
  }
  return(gs)
}


################################################################################
### Rotate
################################################################################

#-------------------------------------------------------------------------------
.rotate_gspace <- function(gs, quarter_turns) {
  if (.has_image(gs)) {
    gs <- .rotate_gspace_image(gs, quarter_turns)
  } else {
    gs <- .rotate_gspace_graph(gs, quarter_turns)
  }
  gs@nodes <- .rotate90_node_geometry(gs@nodes, quarter_turns)
  return(gs)
}

#-------------------------------------------------------------------------------
.rotate_gspace_graph <- function(gs, quarter_turns) {
  nodes <- gs@nodes
  rot <- .rotate90_xy(nodes$x, nodes$y, quarter_turns)
  nodes$x <- rot$x
  nodes$y <- rot$y
  gs@nodes <- nodes
  return(gs)
}

#-------------------------------------------------------------------------------
.rotate_gspace_image <- function(gs, quarter_turns) {
  nodes <- gs@nodes
  rot <- .rotate90_xy(nodes$x, nodes$y, quarter_turns)
  nodes$x <- rot$x
  nodes$y <- rot$y
  gs@nodes <- nodes
  gs@canvas <- .rotate90_image(gs@canvas, quarter_turns)
  return(gs)
}

#-------------------------------------------------------------------------------
# One 90-degree CCW step: (x, y) -> (1 - y, x); applied quarter_turns times.
.rotate90_xy <- function(x, y, quarter_turns) {
  for (i in seq_len(quarter_turns)) {
    x_new <- 1 - y
    y_new <- x
    x <- x_new; y <- y_new
  }
  list(x = x, y = y)
}

#-------------------------------------------------------------------------------
# Rotate the image by quarter-turns, branching on image type:
#   raster:     matrix-based rotation
#   SpatRaster: terra's native lazy rotation
.rotate90_image <- function(img, quarter_turns) {
  if (inherits(img, "SpatRaster")){
    for (i in seq_len(quarter_turns)) {
      img <- terra::flip(terra::trans(img), direction = "vertical")
    }
    return(img)
  }
  m <- as.matrix(img)
  for (i in seq_len(quarter_turns)) {
    m <- t(m)
    m <- m[rev(seq_len(nrow(m))), , drop = FALSE]
  }
  as.raster(m)
}

################################################################################
### Flip
################################################################################

#-------------------------------------------------------------------------------
.flip_gspace <- function(gs, vertical) {
  if (.has_image(gs)) {
    gs <- .flip_gspace_image(gs, vertical)
  } else {
    gs <- .flip_gspace_graph(gs, vertical)
  }
  gs@nodes <- .flip_node_geometry(gs@nodes, vertical)
  return(gs)
}

#-------------------------------------------------------------------------------
.flip_gspace_graph <- function(gs, vertical) {
  
  nodes <- gs@nodes
  if (vertical) {
    nodes$y <- 1 - nodes$y
  } else {
    nodes$x <- 1 - nodes$x
  }
  gs@nodes <- nodes
  
  return(gs)
  
}

#-------------------------------------------------------------------------------
.flip_gspace_image <- function(gs, vertical) {
  
  nodes <- gs@nodes
  if (vertical) {
    nodes$y <- 1 - nodes$y
  } else {
    nodes$x <- 1 - nodes$x
  }
  gs@nodes <- nodes
  gs@canvas <- .flip_image(gs@canvas, vertical)
  
  return(gs)
  
}

#-------------------------------------------------------------------------------
.flip_image <- function(img, vertical) {
  if (inherits(img, "SpatRaster")) {
    img <- terra::flip(img, 
      direction = if (vertical) "vertical" else "horizontal")
    return(img)
  }
  if (vertical){
    img <- img[rev(seq_len(nrow(img))), , drop = FALSE]
  } else {
    img <- img[, rev(seq_len(ncol(img))), drop = FALSE]
  }
  return(img)
}

################################################################################
### Transpose
################################################################################
.transpose_gspace <- function(gs) {
  
  nodes <- gs@nodes
  x <- nodes$x
  nodes$x <- nodes$y
  nodes$y <- x
  nodes <- .transpose_node_geometry(nodes)
  gs@nodes <- nodes
  
  if (.has_image(gs)) {
    gs@canvas <- .transpose_image(gs@canvas)
  }
  
  return(gs)
  
}

#-------------------------------------------------------------------------------
# Transpose the raw @image, branching on type.
.transpose_image <- function(img) {
  if (inherits(img, "SpatRaster")) {
    img <- terra::flip(img, direction = "vertical")
    img <- terra::flip(img, direction = "horizontal")
    img <- terra::trans(img)
    return(img)
  }
  # Reverse both dimensions, then transpose (see roxygen @details above
  # for why a plain t() would not match the node swap).
  n <- nrow(img); m <- ncol(img)
  img <- t( img[rev(seq_len(n)), rev(seq_len(m)), drop = FALSE] )
  return(img)
}

################################################################################
### Node geometry
################################################################################
# Applies the same rotate/flip/transpose transform to every geometry column
# found in @nodes, so any sfc column stays aligned with the transformed
# nodes and canvas. Each column is transformed around its own bounding-box
# center, independently of the others.

#-------------------------------------------------------------------------------
.rotate90_node_geometry <- function(nodes, quarter_turns) {
  if (quarter_turns == 0L) return(nodes)
  # Sign is intentional: sf's own rot(a) vignette helper is CLOCKWISE for +ve a
  M <- matrix(c(0, -1, 1, 0), 2, 2)
  Mk <- diag(2)
  for (i in seq_len(quarter_turns)) Mk <- Mk %*% M
  for (col in .gs_geometry_cols(nodes)) {
    geom <- nodes[[col]]
    center <- .node_geometry_center(geom)
    nodes[[col]] <- .fast_affine_sfc(geom, Mk, center)
  }
  nodes
}

#-------------------------------------------------------------------------------
# Mirror, matching .flip_gspace_graph()/.flip_gspace_image()
.flip_node_geometry <- function(nodes, vertical) {
  M <- if (vertical) matrix(c(1, 0, 0, -1), 2, 2) else matrix(c(-1, 0, 0, 1), 2, 2)
  for (col in .gs_geometry_cols(nodes)) {
    geom <- nodes[[col]]
    center <- .node_geometry_center(geom)
    nodes[[col]] <- .fast_affine_sfc(geom, M, center)
  }
  nodes
}

#-------------------------------------------------------------------------------
# Swaps x and y about the pivot, matching .transpose_gspace().
.transpose_node_geometry <- function(nodes) {
  M <- matrix(c(0, 1, 1, 0), 2, 2)
  for (col in .gs_geometry_cols(nodes)) {
    geom <- nodes[[col]]
    center <- .node_geometry_center(geom)
    nodes[[col]] <- .fast_affine_sfc(geom, M, center)
  }
  nodes
}

#-------------------------------------------------------------------------------
# Midpoint of the bounding box, not the centroid; the pivot rotate/flip/
# transpose transform around.
.node_geometry_center <- function(geom) {
  # bb <- sf::st_bbox(geom)
  # c(mean(bb[c("xmin", "xmax")]), mean(bb[c("ymin", "ymax")]))
  c(0.5, 0.5)
}

#-------------------------------------------------------------------------------
# Recursively applies (coord - center) %*% M + center to every coordinate
# pair inside an sfg object.
.transform_sfg_coords <- function(g, M, center) {
  if (is.matrix(g)) {
    x <- g[, 1] - center[1]
    y <- g[, 2] - center[2]
    # (x y) %*% M, done column-wise to avoid building an intermediate matrix
    g[, 1] <- x * M[1, 1] + y * M[2, 1] + center[1]
    g[, 2] <- x * M[1, 2] + y * M[2, 2] + center[2]
    return(g)
  }
  if (is.list(g)) {
    for (i in seq_along(g)) {
      g[[i]] <- .transform_sfg_coords(g[[i]], M, center)
    }
    return(g)
  }
  if (is.numeric(g) && length(g) >= 2) {
    x <- g[1] - center[1]
    y <- g[2] - center[2]
    g[1] <- x * M[1, 1] + y * M[2, 1] + center[1]
    g[2] <- x * M[1, 2] + y * M[2, 2] + center[2]
    return(g)
  }
  g
}

################################################################################
### Applies an affine transform to an sfc column
################################################################################

#-------------------------------------------------------------------------------
# Applies an affine transform to an sfc column. Fast path mutates coordinates in
# place and reuses the original sfc attributes, avoiding st_sfc()'s O(N) rebuild.
# Falls back to the slower but always-correct method if the fast path fails.
.fast_affine_sfc <- function(geom, M, center) {
  transformed <- tryCatch(
    .affine_sfc_fast(geom, M, center),
    error = function(e) NULL)
  
  if (!.affine_result_ok(transformed, geom)) {
    rlang::warn(c(
      "!" = "Fast geometry transform failed; falling back to the slower method.",
      "i" = "Please report this, ideally with a minimal reproducible example."
    ))
    res <- .affine_sfc_slow(geom, M, center)
    return(res)
  }
  
  transformed
}

#-------------------------------------------------------------------------------
# Fast path: transform coordinates in place, reuse attributes, transform bbox.
.affine_sfc_fast <- function(geom, M, center) {
  new_features <- lapply(unclass(geom),
    .transform_sfg_coords, M = M, center = center)
  attributes(new_features) <- attributes(geom)
  attr(new_features, "bbox") <- .transform_bbox(attr(geom, "bbox"), M, center)
  new_features
}

#-------------------------------------------------------------------------------
# Slow path: sf's own arithmetic. Always correct; used only as a fallback.
.affine_sfc_slow <- function(geom, M, center) {
  (geom - center) * M + center
}

#-------------------------------------------------------------------------------
# Cheap validity check: a rigid transform preserves feature count, so a
# mismatch (or a NULL from a caught error) means the fast path failed.
.affine_result_ok <- function(transformed, geom) {
  !is.null(transformed) && length(transformed) == length(geom)
}

#-------------------------------------------------------------------------------
# Transform a bbox by applying the affine map to its 4 corners (O(1)).
.transform_bbox <- function(bb, M, center) {
  corners <- rbind(
    c(bb["xmin"], bb["ymin"]), c(bb["xmin"], bb["ymax"]),
    c(bb["xmax"], bb["ymin"]), c(bb["xmax"], bb["ymax"]))
  corners[, 1] <- corners[, 1] - center[1]
  corners[, 2] <- corners[, 2] - center[2]
  corners <- corners %*% M
  corners[, 1] <- corners[, 1] + center[1]
  corners[, 2] <- corners[, 2] + center[2]
  structure(
    c(xmin = min(corners[, 1]), ymin = min(corners[, 2]),
      xmax = max(corners[, 1]), ymax = max(corners[, 2])),
    class = "bbox")
}
