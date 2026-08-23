
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
#' @param clockwise A single logical value. If \code{FALSE} (default), the
#' 90-degree turn is counter-clockwise; if \code{TRUE}, clockwise
#' (\code{rotateGraphSpace} only).
#' @param vertical A single logical value. If \code{FALSE} (default), the
#' flip is horizontal (mirror left-right); if \code{TRUE}, vertical 
#' (mirror top-bottom).
#' (\code{flipGraphSpace} only).
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

#' @aliases rotateGraphSpace
#' @rdname GraphSpace-transform
#' @export
setMethod("rotateGraphSpace", "GraphSpace",
  function(gs, clockwise = FALSE, verbose = TRUE){
    
    gs <- updateGraphSpace(gs)
    
    if(!.is_normalized(gs)){
      rlang::abort(
        message = c(
          "The 'GraphSpace' object must be normalized before rotating.",
          "i" = "Please run 'normalizeGraphSpace(gs)' first."
        )
      )
    }
    
    .validate_gs_args("singleLogical", "clockwise", clockwise)
    .validate_gs_args("singleLogical", "verbose", verbose)
    
    quarter_turns <- if(clockwise) 3L else 1L
    
    if(verbose){
      rlang::inform(sprintf("Rotating graph space 90 degrees %s...",
        if(clockwise) "clockwise" else "counter-clockwise"))
    }
    
    gs <- .rotate_gspace(gs, quarter_turns)
    
    return(gs)
    
  })

#' @aliases flipGraphSpace
#' @rdname GraphSpace-transform
#' @export
setMethod("flipGraphSpace", "GraphSpace",
  function(gs, vertical = FALSE, verbose = TRUE){
    
    gs <- updateGraphSpace(gs)
    
    if(!.is_normalized(gs)){
      rlang::abort(
        message = c(
          "The 'GraphSpace' object must be normalized before flipping.",
          "i" = "Please run 'normalizeGraphSpace(gs)' first."
        )
      )
    }
    .validate_gs_args("singleLogical", "vertical", vertical)
    .validate_gs_args("singleLogical", "verbose", verbose)
    
    if(verbose){
      rlang::inform(sprintf("Flipping graph space %s...",
        if(vertical) "vertically" else "horizontally"))
    }
    
    gs <- .flip_gspace(gs, vertical)
    
    return(gs)
    
  })

#' @aliases transposeGraphSpace
#' @rdname GraphSpace-transform
#' @export
setMethod("transposeGraphSpace", "GraphSpace",
  function(gs, verbose = TRUE){
    
    gs <- updateGraphSpace(gs)
    
    if(!.is_normalized(gs)){
      rlang::abort(
        message = c(
          "The 'GraphSpace' object must be normalized before transposing.",
          "i" = "Please run 'normalizeGraphSpace(gs)' first."
        )
      )
    }
    .validate_gs_args("singleLogical", "verbose", verbose)
    
    if(verbose) rlang::inform("Transposing graph space...")
    
    gs <- .transpose_gspace(gs)
    
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
  gs@canvas <- .rotate90_canvas(gs@canvas, quarter_turns)
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
# One 90-degree CCW step on the canvas, matching .rotate90_xy(); applied
# quarter_turns times.
.rotate90_canvas <- function(canvas, quarter_turns) {
  for (i in seq_len(quarter_turns)) {
    Tc <- t(canvas)
    canvas <- Tc[rev(seq_len(nrow(Tc))), , drop = FALSE]
  }
  canvas
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
  canvas <- gs@canvas
  
  if (vertical) {
    nodes$y <- 1 - nodes$y
    canvas <- canvas[rev(seq_len(nrow(canvas))), , drop = FALSE]
  } else {
    nodes$x <- 1 - nodes$x
    canvas <- canvas[, rev(seq_len(ncol(canvas))), drop = FALSE]
  }
  
  gs@nodes <- nodes
  gs@canvas <- canvas
  
  return(gs)
  
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
    canvas <- gs@canvas
    n <- nrow(canvas); m <- ncol(canvas)
    # Reverse both dimensions, then transpose (see roxygen @details above
    # for why a plain t() would not match the node swap).
    gs@canvas <- t(canvas[rev(seq_len(n)), rev(seq_len(m)), drop = FALSE])
  }
  
  return(gs)
  
}

################################################################################
### Node geometry (optional; runs on every sfc-typed column in @nodes)
################################################################################
# Applies the same rotate/flip/transpose transform to every geometry column
# found in @nodes, so any sfc column stays aligned with the transformed
# nodes and canvas. Each column is transformed around its own bounding-box
# center, independently of the others.

#-------------------------------------------------------------------------------
# Sign is intentional: sf's own rot(a) vignette helper is CLOCKWISE for
# positive a; this matrix is the CCW equivalent, matching the rest of this file.
.rotate90_node_geometry <- function(nodes, quarter_turns) {
  if (quarter_turns == 0L) return(nodes)
  # Sign is intentional: sf's own rot(a) vignette helper is CLOCKWISE for
  # positive a; this matrix is the CCW equivalent, matching the rest of this file.
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
# Mirror, matching .flip_gspace_graph()/.flip_gspace_image(): negate x
# (horizontal) or y (vertical) about the pivot.
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
# Swaps x and y about the pivot, matching .transpose_gspace(). The swap
# matrix is symmetric, so unlike rotate/flip there's no way to pick the
# wrong left/right-multiply convention.
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
  bb <- sf::st_bbox(geom)
  c(mean(bb[c("xmin", "xmax")]), mean(bb[c("ymin", "ymax")]))
}

#-------------------------------------------------------------------------------
# Applies an affine transform to an sfc column, checks the result, and
# falls back to a slower but always-correct method if anything looks wrong.
.fast_affine_sfc <- function(geom, M, center) {
  n_before <- length(geom)
  type_before <- sf::st_geometry_type(geom, by_geometry = TRUE)
  
  transformed <- tryCatch({
    new_features <- unname(lapply(unclass(geom), 
      .transform_sfg_coords, M = M, center = center))
    sf::st_sfc(new_features, crs = sf::st_crs(geom))
  }, error = function(e) NULL)
  
  ok <- !is.null(transformed) &&
    length(transformed) == n_before &&
    identical(as.character(sf::st_geometry_type(transformed, by_geometry = TRUE)),
      as.character(type_before))
  
  if (!ok) {
    rlang::warn(c(
      "!" = "Fast geometry transform failed validation; falling back to the slower method.",
      "i" = "Please report this, ideally with a minimal reproducible example."
    ))
    return((geom - center) * M + center)
  }
  
  transformed
}

#-------------------------------------------------------------------------------
# Recursively applies (coord - center) %*% M + center to every coordinate
# pair inside an sfg object, however deeply nested.
.transform_sfg_coords <- function(g, M, center) {
  if (is.matrix(g)) {
    xy <- g[, 1:2, drop = FALSE]
    xy <- sweep(xy, 2, center, "-")
    xy <- xy %*% M
    xy <- sweep(xy, 2, center, "+")
    g[, 1:2] <- xy
    return(g)
  }
  if (is.list(g)) {
    for (i in seq_along(g)) {
      g[[i]] <- .transform_sfg_coords(g[[i]], M, center)
    }
    return(g)
  }
  if (is.numeric(g) && length(g) >= 2) {
    xy <- as.vector((g[1:2] - center) %*% M) + center
    g[1:2] <- xy
    return(g)
  }
  g
}

