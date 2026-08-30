
################################################################################
### ROTATE
################################################################################

#-------------------------------------------------------------------------------
.rotate_gspace_raw <- function(gs, quarter_turns) {
  .assert_raw_transform_ok(gs, "rotate")
  
  if (.has_image(gs)) {
    gs <- .rotate_raw_image(gs, quarter_turns)
  } else {
    gs <- .rotate_raw_graph(gs, quarter_turns)
  }
  
  gs <- .denormalize_after_raw(gs)
  gs
}

#-------------------------------------------------------------------------------
# IMAGE case: node coordinates are 1-based pixel indices (x=col, y=row).
.rotate_raw_image <- function(gs, quarter_turns) {
  x <- gs@coords$x
  y <- gs@coords$y
  nrow_img <- nrow(gs@image)
  ncol_img <- ncol(gs@image)
  
  for (i in seq_len(quarter_turns)) {
    x_new <- y
    y_new <- ncol_img - x + 1
    x <- x_new; y <- y_new
    # dims swap for the next quarter turn
    tmp <- nrow_img; nrow_img <- ncol_img; ncol_img <- tmp
  }
  
  gs <- .set_raw_xy(gs, x, y)
  gs@image <- .rotate90_image(gs@image, quarter_turns)
  gs@coords <- .rotate90_coords_geometry(gs@coords, quarter_turns)
  gs
}

#-------------------------------------------------------------------------------
# GRAPH-only case (no image): rotate about the coordinate extent center.
.rotate_raw_graph <- function(gs, quarter_turns) {
  piv <- .raw_pivot(gs)
  xy  <- .rotate90_xy_raw(gs@coords$x, gs@coords$y, quarter_turns, piv)
  gs  <- .set_raw_xy(gs, xy$x, xy$y)
  gs@coords <- .rotate90_coords_geometry(gs@coords, quarter_turns, piv)
  gs
}

################################################################################
### FLIP  (image node math uses pixel-index; confirmed against real image dims)
################################################################################

#-------------------------------------------------------------------------------
.flip_gspace_raw <- function(gs, vertical) {
  
  .assert_raw_transform_ok(gs, "flip")

  if (.has_image(gs)) {
    gs <- .flip_raw_image(gs, vertical)
  } else {
    gs <- .flip_raw_graph(gs, vertical)
  }
  
  gs <- .denormalize_after_raw(gs)
  gs
}

#-------------------------------------------------------------------------------
# IMAGE case: nodes are 1-based pixel indices (x=col, y=row).
#   vertical   (mirror top-bottom, reverse rows): y_new = nrow - y + 1
#   horizontal (mirror left-right, reverse cols): x_new = ncol - x + 1
.flip_raw_image <- function(gs, vertical) {
  nr <- nrow(gs@image)
  nc <- ncol(gs@image)
  
  if (vertical) {
    gs <- .set_raw_xy(gs, gs@coords$x, nr - gs@coords$y + 1)
  } else {
    gs <- .set_raw_xy(gs, nc - gs@coords$x + 1, gs@coords$y)
  }
  gs@image <- .flip_image(gs@image, vertical)
  gs@coords <- .flip_coords_geometry(gs@coords, vertical)
  gs
}

#-------------------------------------------------------------------------------
# GRAPH-only case (no image): mirror about the coordinate extent center.
.flip_raw_graph <- function(gs, vertical) {
  piv <- .raw_pivot(gs)
  if (vertical) {
    gs <- .set_raw_xy(gs, gs@coords$x, 2 * piv$y - gs@coords$y)
  } else {
    gs <- .set_raw_xy(gs, 2 * piv$x - gs@coords$x, gs@coords$y)
  }
  gs@coords <- .flip_coords_geometry(gs@coords, vertical, piv)
  gs
}

################################################################################
### TRANSPOSE  (image node math uses pixel-index)
################################################################################

#-------------------------------------------------------------------------------
.transpose_gspace_raw <- function(gs) {
  
  .assert_raw_transform_ok(gs, "transpose")
  
  if (.has_image(gs)) {
    gs <- .transpose_raw_image(gs)
  } else {
    gs <- .transpose_raw_graph(gs)
  }
  
  gs <- .denormalize_after_raw(gs)
  gs
}

#-------------------------------------------------------------------------------
# IMAGE case: nodes are 1-based pixel indices (x=col, y=row).
# transposed pixel; in x/y this is:
#   x_new = nrow - y + 1
#   y_new = ncol - x + 1
.transpose_raw_image <- function(gs) {
  nr <- nrow(gs@image)
  nc <- ncol(gs@image)
  
  new_x <- nr - gs@coords$y + 1
  new_y <- nc - gs@coords$x + 1
  gs <- .set_raw_xy(gs, new_x, new_y)
  
  gs@image <- .transpose_image(gs@image)
  gs@coords <- .transpose_coords_geometry(gs@coords)
  gs
}

#-------------------------------------------------------------------------------
# GRAPH-only case (no image): swap x/y about the coordinate extent center.
.transpose_raw_graph <- function(gs) {
  piv <- .raw_pivot(gs)
  new_x <- piv$x + (gs@coords$y - piv$y)
  new_y <- piv$y + (gs@coords$x - piv$x)
  gs <- .set_raw_xy(gs, new_x, new_y)
  gs@coords <- .transpose_coords_geometry(gs@coords, piv)
  gs
}

################################################################################
### Shared helpers
################################################################################

#-------------------------------------------------------------------------------
.assert_raw_transform_ok <- function(gs, what) {
  if (nrow(gs@coords) == 0L) {
    rlang::abort(c(
      sprintf("Cannot %s: the 'GraphSpace' object has no raw coordinates.", what),
      "i" = "Rebuild the object with 'GraphSpace()'."
    ))
  }
  invisible(TRUE)
}

#-------------------------------------------------------------------------------
# Pivot for the GRAPH-only raw transforms (coordinate extent center).
.raw_pivot <- function(gs) {
  list(
    x = (min(gs@coords$x) + max(gs@coords$x)) / 2,
    y = (min(gs@coords$y) + max(gs@coords$y)) / 2
  )
}

#-------------------------------------------------------------------------------
# One 90-deg CCW step about an arbitrary pivot (graph-only path).
.rotate90_xy_raw <- function(x, y, quarter_turns, piv) {
  for (i in seq_len(quarter_turns)) {
    dx <- x - piv$x
    dy <- y - piv$y
    x_new <- piv$x - dy
    y_new <- piv$y + dx
    x <- x_new; y <- y_new
  }
  list(x = x, y = y)
}

#-------------------------------------------------------------------------------
# Write x/y into BOTH @coords and V(graph), keeping them in agreement.
.set_raw_xy <- function(gs, x, y) {
  gs@coords$x <- x
  gs@coords$y <- y
  sf <- gs_scale_factor(gs)
  gs@graph <- igraph::set_vertex_attr(gs@graph, "x", value = x / sf)
  gs@graph <- igraph::set_vertex_attr(gs@graph, "y", value = y / sf)
  gs
}

#-------------------------------------------------------------------------------
# Drop derived/normalized state so the next normalizeGraphSpace() rebuilds
# @nodes from the transformed raw sources.
.denormalize_after_raw <- function(gs) {
  gs@pars$is.normalized <- FALSE
  gs@pars$image.space   <- FALSE
  gs@canvas <- as.raster(matrix())
  gs@nodes <- .set_raw_coords(gs@nodes, gs@coords)
  for (col in .gs_geometry_cols(gs@coords)) {
    gs@nodes[[col]] <- gs@coords[[col]]
  }
  gs
}

################################################################################
### Raw geometry-column transforms
################################################################################

#-------------------------------------------------------------------------------
# Geometry-column transforms; `piv` is the rotation/reflection pivot
.geometry_pivot <- function(geom, piv) {
  if (!is.null(piv)) return(c(piv$x, piv$y))
  bb <- sf::st_bbox(geom)
  c((bb["xmin"] + bb["xmax"]) / 2, (bb["ymin"] + bb["ymax"]) / 2)
}

#-------------------------------------------------------------------------------
.rotate90_coords_geometry <- function(coords, quarter_turns, piv = NULL) {
  if (quarter_turns == 0L) return(coords)
  M <- matrix(c(0, -1, 1, 0), 2, 2)
  Mk <- diag(2)
  for (i in seq_len(quarter_turns)) Mk <- Mk %*% M
  for (col in .gs_geometry_cols(coords)) {
    center <- .geometry_pivot(coords[[col]], piv)
    coords[[col]] <- .fast_affine_sfc(coords[[col]], Mk, center)
  }
  coords
}

#-------------------------------------------------------------------------------
.flip_coords_geometry <- function(coords, vertical, piv = NULL) {
  M <- if (vertical) matrix(c(1, 0, 0, -1), 2, 2) else matrix(c(-1, 0, 0, 1), 2, 2)
  for (col in .gs_geometry_cols(coords)) {
    center <- .geometry_pivot(coords[[col]], piv)
    coords[[col]] <- .fast_affine_sfc(coords[[col]], M, center)
  }
  coords
}

#-------------------------------------------------------------------------------
.transpose_coords_geometry <- function(coords, piv = NULL) {
  M <- matrix(c(0, 1, 1, 0), 2, 2)
  for (col in .gs_geometry_cols(coords)) {
    center <- .geometry_pivot(coords[[col]], piv)
    coords[[col]] <- .fast_affine_sfc(coords[[col]], M, center)
  }
  coords
}
