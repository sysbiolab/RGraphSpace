
#-------------------------------------------------------------------------------
#' Annotate a GraphSpace Plot with an Image
#'
#' @description
#' \code{annotation_gspace_image()} adds an image annotation layer to a
#' \code{ggplot}-based \code{GraphSpace} plot.
#'
#' @param x An image to be displayed. Accepted types:
#' \itemize{
#'   \item A \code{\link{GraphSpace}} object — the image is extracted via
#'     \code{\link{gs_image}}.
#'   \item A \code{\link[terra]{SpatRaster}}) object.
#'   \item A \code{raster} object (see \code{\link[grDevices]{as.raster}}).
#'   \item A \code{matrix} or 3D array (RGB/RGBA), coerced to
#'     \code{raster} automatically.
#' }
#' @param interpolate A logical value indicating whether to apply linear
#' interpolation when the image is rendered at a different resolution than
#' its native size. Defaults to \code{FALSE}.
#' @param opacity A numeric value in \code{[0, 1]} controlling the
#' transparency of the image. \code{1} is fully opaque (default);
#' \code{0} is fully transparent.
#' @param flip.v A logical value; if \code{TRUE}, the image is flipped
#' vertically (top-to-bottom). Defaults to \code{FALSE}.
#' @param flip.h A logical value; if \code{TRUE}, the image is flipped
#' horizontally (left-to-right). Defaults to \code{FALSE}.
#' @param na.color The colour to map to NA values. Defaults to \code{NA}.
#' @param rgb_channels When a \code{\link[terra]{SpatRaster}} is provided,
#' an integer vector of length 3 giving the layers to use as the red, green,
#' and blue channels. Use \code{NA} for an empty channel (e.g.
#' \code{c(3, 2, NA)}). Defaults to \code{c(1, 2, 3)}.
#' @param stretch When a \code{\link[terra]{SpatRaster}} is provided, option to 
#' stretch RGB values to increase contrast: "lin" (linear) or "hist" (histogram).
#' To disable, set \code{stretch = NULL}. See \code{\link[terra]{plotRGB}}.
#' @return A ggplot2 layer object that can be added to a \code{ggplot()}
#' call with \code{+}, or \code{invisible(NULL)} with a warning if the
#' image could not be resolved.
#'
#' @seealso
#' \code{\link[ggplot2]{annotation_raster}},
#' \code{\link{gs_image}},
#' \code{\link{geom_nodespace}},
#' \code{\link{geom_edgespace}}
#'
#' @examples
#' 
#' library(RGraphSpace)
#' library(igraph)
#' 
#' # Load a demo igraph
#' data('gtoy1', package = 'RGraphSpace')
#' gs <- GraphSpace(gtoy1)
#' 
#' # Normalize node coordinates
#' gs <- normalizeGraphSpace(gs)
#' 
#' # Add a raster image
#' gs_image(gs) <- as_colorraster(volcano)
#' 
#' \dontrun{
#' # Pass a GraphSpace object directly
#' ggplot(gs) +
#'   annotation_gspace_image(gs) +
#'   geom_edgespace() +
#'   geom_nodespace()
#'
#' # Extract the image explicitly
#' ggplot(gs) +
#'   annotation_gspace_image(gs_image(gs)) +
#'   geom_edgespace() +
#'   geom_nodespace()
#'
#' # Dim the background and flip vertically
#' ggplot(gs) +
#'   annotation_gspace_image(gs, opacity = 0.5, flip.v = TRUE) +
#'   geom_edgespace() +
#'   geom_nodespace()
#'   
#' }
#' @importFrom ggplot2 annotation_raster
#' @importFrom grDevices col2rgb rgb
#' @rdname annotation_gspace_image
#' @export
annotation_gspace_image <- function(x, interpolate = FALSE, 
  opacity = 1, flip.v = FALSE, flip.h = FALSE, na.color = NA, 
  rgb_channels = c(1, 2, 3), stretch = c("lin", "hist")) {

  if (missing(x)) {
    rlang::abort("Argument 'x' is missing, with no default.")
  }
  
  .validate_gs_args("singleLogical", "interpolate", interpolate)
  .validate_gs_args("singleLogical", "flip.v", flip.v)
  .validate_gs_args("singleLogical", "flip.h", flip.h)
  .validate_gs_args("singleNumber", "opacity", opacity)
  .validate_gs_args("integer_vec", "rgb_channels",
    rgb_channels, notNA = FALSE)
  if(!is.null(stretch)){
    stretch <- match.arg(stretch, c("lin", "hist"))
  }
  
  if(!is.na(na.color)){
    .validate_gs_colors("singleColor", "na.color", na.color)
  }
  
  if (inherits(x, "GraphSpace")) {
    if (!.has_image(x)) {
      rlang::warn("The 'GraphSpace' object contains no image.")
      return(invisible(NULL))
    }
    mp <- gs_image_maxpixels(x)
    x <- gs_image(x)
  } else {
    mp <- 4e6
  }
  
  if (inherits(x, "SpatRaster")) {
    x <- .spatraster_to_raster(x, maxpixels = mp, 
      rgb_channels = rgb_channels, stretch = stretch)
  } else if (!inherits(x, "raster")) {
    x <- tryCatch({
      grDevices::as.raster(x)
    }, error = function(e) {
      rlang::warn(c(
        "x" = "Failed to convert 'x' to a valid raster object.",
        "i" = "Accepted types: raster, SpatRaster, matrix, or array (RGB/RGBA)."
      ))
      NULL
    })
    if (is.null(x)) return(invisible(NULL))
  }

  if (opacity < 0 || opacity > 1) {
    rlang::warn(c(
      "'opacity' must be between 0 and 1.",
      "i" = sprintf("Value %s was clamped to %s.", 
        opacity, max(0, min(1, opacity)))
    ))
    opacity <- max(0, min(1, opacity))
  }
  
  if (opacity != 1) {
    img <- grDevices::col2rgb(as.character(x), alpha = TRUE)
    img[4, ] <- as.integer(opacity * 255)
    x <- as.raster(matrix(
      grDevices::rgb(img[1,], img[2,], img[3,], img[4,], maxColorValue = 255),
      nrow = nrow(x), ncol = ncol(x),
      byrow = TRUE))
    # Note: byrow = TRUE is INTENTIONAL, not a bug: as.character() on a "raster"
    # object does NOT flatten like a plain matrix (verified: identical(
    # as.character(as.raster(m)), as.character(m)) is FALSE for identical m).
    # byrow = TRUE correctly reverses that reordering. Confirmed visually.
  }

  if (flip.v) x <- x[rev(seq_len(nrow(x))), , drop = FALSE]
  if (flip.h) x <- x[, rev(seq_len(ncol(x))), drop = FALSE]
  if (!is.na(na.color)) x[is.na(x)] <- na.color
  
  ggplot2::annotation_raster(raster = x,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    interpolate = interpolate)
  
}

#-------------------------------------------------------------------------------
# Convert a (multi-band) SpatRaster window to a base R raster of colour strings.
.spatraster_to_raster <- function(x, maxpixels = 4e6, 
  rgb_channels = c(1, 2, 3), stretch = "lin") {
  
  nb_src <- terra::nlyr(x)
  rgb_channels <- as.integer(rgb_channels)
  if (length(rgb_channels) != 3L)
    stop("'rgb_channels' must have length 3 (R, G, B); use NA for an empty channel")
  valid <- rgb_channels[!is.na(rgb_channels)]
  if (length(valid) == 0L)
    stop("at least one of the R, G, B channels must be a layer index")
  if (any(valid < 1L | valid > nb_src))
    stop("'rgb_channels' index out of range (image has ", nb_src, " layers)")
  
  if (terra::ncell(x) > maxpixels) {
    s <- sqrt(maxpixels / terra::ncell(x))
    target <- terra::rast(nrows = max(1L, floor(nrow(x) * s)),
      ncols = max(1L, floor(ncol(x) * s)),
      extent = terra::ext(x), crs = terra::crs(x))
    x <- terra::resample(x, target, method = "bilinear")
  }
  
  if (!is.null(stretch)) {
    if (stretch == "lin") {
      x <- terra::stretch(x, minv = 0, maxv = 255, minq = 0.02, maxq = 0.98)
    } else {
      x <- terra::stretch(x, minv = 0, maxv = 255, histeq = TRUE)
    }
  }
  
  arr <- terra::as.array(x)
  if (length(dim(arr)) == 2L) dim(arr) <- c(dim(arr), 1L)
  nr <- dim(arr)[1]; nc <- dim(arr)[2]
  
  pick <- function(idx) if (is.na(idx)) matrix(0, nr, nc) else arr[, , idx]
  R <- pick(rgb_channels[1]); G <- pick(rgb_channels[2]); B <- pick(rgb_channels[3])
  
  # normalize to [0,1]: values >1 are assumed to be on a 0-255 scale
  if (!is.null(stretch)) {
    # stretch guaranteed 0-255 -> divide by 255 deterministically
    R <- R/255; G <- G/255; B <- B/255
  } else {
    # no stretch: normalize by observed max (handles any input scale)
    m <- max(c(R, G, B), na.rm = TRUE)
    if (is.finite(m) && m > 1) { R <- R/m; G <- G/m; B <- B/m }
  }
  
  na_cell <- !is.finite(R) & !is.finite(G) & !is.finite(B)
  clamp <- function(p) { p[!is.finite(p)] <- 0; pmin(pmax(p, 0), 1) }
  R <- clamp(R); G <- clamp(G); B <- clamp(B)
  
  out <- grDevices::as.raster(array(c(R, G, B), dim = c(nr, nc, 3)))
  out[na_cell] <- NA
  out
}

#-------------------------------------------------------------------------------
# Convert a base R raster to SpatRaster
.raster_to_spatraster <- function(r) {
  m <- as.matrix(r)              # char matrix of "#RRGGBB"
  rgb <- grDevices::col2rgb(m)   # 3 x N matrix (R,G,B rows), 0-255
  nr <- nrow(m); nc <- ncol(m)
  # build a 3-band SpatRaster
  arr <- array(0, dim = c(nr, nc, 3))
  arr[,,1] <- matrix(rgb[1,], nr, nc)
  arr[,,2] <- matrix(rgb[2,], nr, nc)
  arr[,,3] <- matrix(rgb[3,], nr, nc)
  terra::rast(arr)
}

#-------------------------------------------------------------------------------
# Convert a (multi-band) SpatRaster window to a base R raster of colour strings.
# .spatraster_to_raster <- function(x, maxpixels = 4e6) {
#   
#   if (terra::ncell(x) > maxpixels) {
#     scale  <- sqrt(maxpixels / terra::ncell(x))
#     target <- terra::rast(nrows = max(1L, floor(nrow(x) * scale)),
#       ncols = max(1L, floor(ncol(x) * scale)),
#       extent = terra::ext(x), crs = terra::crs(x))
#     x <- terra::resample(x, target, method = "bilinear")
#   }
#   
#   a  <- terra::as.array(x)
#   if (length(dim(a)) == 2L) dim(a) <- c(dim(a), 1L)
#   nb <- dim(a)[3]
#   
#   # scale to [0, 1] by the GLOBAL max across all bands -- preserves channel
#   # balance for RGB display; per-band scaling would shift colour. Values
#   # already within [0, 1] are treated as display-ready and left as-is.
#   m <- max(a, na.rm = TRUE)
#   if (is.finite(m) && m > 1) a <- a / m
#   
#   # Cells non-finite (e.g. NA padding) must stay NA so they render transparent
#   nf <- !is.finite(a)
#   na_cell <- apply(nf, c(1, 2), all)
#   a[nf] <- 0
#   a[] <- pmin(pmax(a, 0), 1)
#   
#   r <- if (nb == 1) {
#     grDevices::as.raster(a[, , 1])
#   } else if (nb == 2) {
#     # as.raster needs 1, 3, or 4 planes; a 2-band array errors. Promote to RGB
#     # with an empty blue channel: band1 -> R, band2 -> G. (a is already clamped
#     # to [0,1] and NA-zeroed above, so these planes are display-ready)
#     rgb <- array(0, dim = c(dim(a)[1], dim(a)[2], 3))
#     rgb[, , 1:2] <- a[, , 1:2]
#     grDevices::as.raster(rgb)
#   } else {
#     grDevices::as.raster(a[, , seq_len(min(nb, 3)), drop = FALSE])
#   }
#   r[na_cell] <- NA
#   r
# }


