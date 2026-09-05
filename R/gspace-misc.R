
################################################################################
### Package documentation
################################################################################
#' @details
#' 
#' For a hands-on introduction, see the vignette:
#' \code{vignette("RGraphSpace")}.
#' 
#' The full set of documented topics can also be browsed in HTML by
#' running \code{help.start()} and selecting the RGraphSpace package
#' from the package list.
#' 
#' @references
#' `r paste(format(citation("RGraphSpace"), style = "text"), collapse = "\n\n")`
#' 
#' @aliases RGraphSpace-package
#' @keywords internal
#' 
#' @importFrom lifecycle deprecated deprecate_soft is_present
"_PACKAGE"


################################################################################
### Documentation for some 'toy' datasets
################################################################################

#-------------------------------------------------------------------------------
#' @title Toy 'igraph' objects
#'
#' @description Small 'igraph' objects used for workflow demonstrations.
#' All graphs include 'x', 'y', and 'name' vertex attributes.
#'
#' @format igraph
#'
#' @usage 
#' data(gtoy1)
#' data(gtoy2)
#' 
#' @source This package.
#'
#' @docType data
#' @keywords datasets
#' @name gtoys
#' @aliases gtoy1
#' @aliases gtoy2
#' @return A pre-processed igraph object.
#' @examples
#' library(RGraphSpace)
#' data(gtoy1)
#' data(gtoy2)
NULL

#-------------------------------------------------------------------------------
#' @title Toy 'GraphSpace' object
#'
#' @description A small \code{GraphSpace} object used for workflow demonstrations.
#' It includes an embedded image, with node coordinates representing image indices.
#'
#' @format An \link{GraphSpace} object ready for rendering.
#'
#' @usage data(gs_image_toy)
#'
#' @source This package.
#'
#' @docType data
#' @keywords datasets
#' @name gs_image_toy
#' @return A pre-processed \link{GraphSpace} object.
#' @examples
#' library(RGraphSpace)
#' data(gs_image_toy)
NULL

#-------------------------------------------------------------------------------
#' Map numeric values to a color raster
#'
#' Helper function that converts numeric values to colors and 
#' returns a raster image. Useful for visualizing numeric matrices 
#' as color backgrounds.
#'
#' @param x A numeric vector or matrix containing values to be mapped to colors.
#' @param palette A vector of colors used as the palette. By default,
#'   \code{hcl.colors(30)} is used.
#' @param na.color Color used for \code{NA} values. Defaults to \code{white}.
#'
#' @details
#' Values in \code{x} are rescaled to the range of the palette using
#' \code{scales::rescale()}, and each value is mapped to a corresponding
#' color. If \code{x} is a matrix, the resulting raster preserves the same
#' dimensions.
#'
#' @return A raster object as produced by \code{as.raster()}.
#'
#' @examples
#' library(RGraphSpace)
#' 
#' # Convert the volcano matrix to a color raster
#' img <- as_colorraster(volcano)
#' plot(img)
#'
#' @importFrom grDevices hcl.colors
#' @export
as_colorraster <- function(x, palette = hcl.colors(30), na.color = "white") {
  
  if(!is.numeric(x)){
    stop("'x' must be a numeric vector or matrix.", call. = FALSE)
  }
  .validate_gs_colors("allColors", "palette", palette)
  .validate_gs_colors("singleColor", "na.color", na.color)
  
  # NOTE: rescale() does NOT divide by zero on constant input; it
  # has an explicit zero_range(from) guard returning mean(to) directly,
  # before any division happens.
  z <- scales::rescale(x, to = c(1, length(palette)))
  z <- pmin(pmax(round(z), 1), length(palette))
  
  m <- palette[z]
  m[is.na(x)] <- na.color
  
  if (is.matrix(x)) {
    dim(m) <- dim(x)
  }
  
  as.raster(m)

}

#-------------------------------------------------------------------------------
#' Build regular polygons
#'
#' Construct one or more regular polygons (equal sides and angles) as
#' \code{sfg}/\code{sfc} \code{POLYGON} geometry. \code{sfshape_ngon()}
#' builds a single polygon at a given center; \code{sfshape_ngons()} builds
#' \code{n} polygons, automatically arranged in a compact, non-overlapping
#' grid. Useful for building varied, non-hand-typed node geometries; see
#' the geometry vignette for examples, and \code{\link{sfshape_stars}} for
#' the star-shaped equivalent.
#'
#' @param cx,cy Numeric. Coordinates of the polygon's center.
#'   (\code{sfshape_ngon()} only.)
#' @param sides Integer. Number of sides; must be 3 or more. For
#'   \code{sfshape_ngons()}, may be a vector, recycled across the \code{n}
#'   polygons to vary shape per polygon.
#' @param radius Numeric. Circumradius -- distance from the center to each
#'   vertex. For \code{sfshape_ngons()}, may be a vector, recycled across
#'   the \code{n} polygons.
#' @param n Integer. Number of polygons to build. (\code{sfshape_ngons()}
#'   only.)
#' @param spacing Numeric. Distance between polygon centers in the
#'   auto-generated grid. Defaults to \code{max(radius) * 2.5}, which
#'   guarantees no overlap. (\code{sfshape_ngons()} only.)
#'
#' @return \code{sfshape_ngon()} returns a single \code{sfg} object of type
#'   \code{POLYGON}. \code{sfshape_ngons()} returns an \code{sfc} of
#'   \code{n} such polygons.
#'
#' @examplesIf requireNamespace("sf", quietly = TRUE)
#' pentagon <- sfshape_ngon(0, 0, sides = 5, radius = 1)
#' hexagon  <- sfshape_ngon(2, 0, sides = 6, radius = 1)
#' plot(sf::st_sfc(pentagon, hexagon))
#'
#' many <- sfshape_ngons(7, sides = c(3, 5, 8), radius = 0.4)
#' plot(many)
#'
#' @seealso \code{\link{sfshape_stars}}
#' @rdname sfshape_ngons
#' @export
sfshape_ngon <- function(cx = 0, cy = 0, sides = 5, radius = 1) {
  
  if (sides < 3) stop("A polygon needs at least 3 sides.")
  
  .gs_require_sf("building geometries")
  
  theta <- seq(0, 2*pi, length.out = sides + 1)[1:sides]
  x <- cx + radius * sin(theta)
  y <- cy + radius * cos(theta)
  x <- c(x, x[1]); y <- c(y, y[1])
  sf::st_polygon(list(cbind(x, y)))
}

#' @rdname sfshape_ngons
#' @export
sfshape_ngons <- function(n, sides = c(3, 5, 7), radius = 0.3, spacing = NULL) {
  
  .gs_require_sf("building geometries")
  
  sides  <- rep_len(sides, n)
  radius <- rep_len(radius, n)
  if (is.null(spacing)) spacing <- max(radius) * 2.5
  pos <- .grid_positions(n, spacing)
  geoms <- Map(sfshape_ngon, pos$cx, pos$cy, sides, radius)
  sf::st_sfc(geoms)
}

#-------------------------------------------------------------------------------
# Shared grid-positioning helper for the *_polygons() wrappers: a compact,
# roughly-square layout (ncol = ceiling(sqrt(n))), spaced far enough apart
# to avoid overlap given the caller-supplied spacing.
.grid_positions <- function(n, spacing) {
  ncol <- ceiling(sqrt(n))
  col  <- (seq_len(n) - 1) %% ncol
  row  <- (seq_len(n) - 1) %/% ncol
  list(cx = col * spacing, cy = -row * spacing)
}

#-------------------------------------------------------------------------------
#' Build star polygons
#'
#' Construct one or more star-shaped polygons, alternating between an
#' outer and an inner radius at each vertex, as \code{sfg}/\code{sfc}
#' \code{POLYGON} geometry. \code{sfshape_star()} builds a single star at
#' a given center; \code{sfshape_stars()} builds \code{n} stars,
#' automatically arranged in a compact, non-overlapping grid. Useful for
#' building varied, non-hand-typed node geometries; see the geometry
#' vignette for examples, and \code{\link{sfshape_ngons}} for the
#' regular-polygon equivalent.
#'
#' @param cx,cy Numeric. Coordinates of the star's center.
#'   (\code{sfshape_star()} only.)
#' @param points Integer. Number of star points; must be 2 or more. For
#'   \code{sfshape_stars()}, may be a vector, recycled across the \code{n}
#'   stars to vary shape per star.
#' @param r_outer Numeric. Radius to each outer (point) vertex. For
#'   \code{sfshape_stars()}, may be a vector, recycled across the \code{n}
#'   stars.
#' @param r_inner Numeric. Radius to each inner (valley) vertex. Smaller
#'   values relative to \code{r_outer} produce sharper points; values
#'   closer to \code{r_outer} produce a rounder, less pronounced star. For
#'   \code{sfshape_stars()}, may be a vector, recycled across the \code{n}
#'   stars.
#' @param n Integer. Number of stars to build. (\code{sfshape_stars()}
#'   only.)
#' @param spacing Numeric. Distance between star centers in the
#'   auto-generated grid. Defaults to \code{max(r_outer) * 2.5}, which
#'   guarantees no overlap. (\code{sfshape_stars()} only.)
#'
#' @return \code{sfshape_star()} returns a single \code{sfg} object of type
#'   \code{POLYGON}. \code{sfshape_stars()} returns an \code{sfc} of
#'   \code{n} such stars.
#'
#' @examplesIf requireNamespace("sf", quietly = TRUE)
#' star5 <- sfshape_star(0, 0, points = 5, r_outer = 1, r_inner = 0.4)
#' star8 <- sfshape_star(3, 0, points = 8, r_outer = 1, r_inner = 0.7)
#' plot(sf::st_sfc(star5, star8))
#'
#' many <- sfshape_stars(6, points = 5, r_outer = 0.4, r_inner = 0.16)
#' plot(many)
#'
#' @seealso \code{\link{sfshape_ngons}}
#' @rdname sfshape_stars
#' @export
sfshape_star <- function(cx = 0, cy = 0, points = 5, 
  r_outer = 0.3, r_inner = 0.1) {
  
  if (points < 2) stop("A star needs at least 2 points.")
  
  .gs_require_sf("building geometries")
  
  n <- points * 2
  theta <- seq(0, 2*pi, length.out = n + 1)[1:n]
  r <- rep(c(r_outer, r_inner), length.out = n)
  x <- cx + r * sin(theta)
  y <- cy + r * cos(theta)
  x <- c(x, x[1]); y <- c(y, y[1])
  sf::st_polygon(list(cbind(x, y)))
}

#' @rdname sfshape_stars
#' @export
sfshape_stars <- function(n, points = c(3, 4, 5), r_outer = 0.3, 
  r_inner = 0.1, spacing = NULL) {
  
  .gs_require_sf("building geometries")
  
  points  <- rep_len(points, n)
  r_outer <- rep_len(r_outer, n)
  r_inner <- rep_len(r_inner, n)
  if (is.null(spacing)) spacing <- max(r_outer) * 2.5
  pos <- .grid_positions(n, spacing)
  geoms <- Map(sfshape_star, pos$cx, pos$cy, points, r_outer, r_inner)
  sf::st_sfc(geoms)
}
