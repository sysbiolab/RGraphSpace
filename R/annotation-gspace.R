
#-------------------------------------------------------------------------------
#' Annotate a GraphSpace Plot with an Image
#'
#' @description
#' \code{annotation_gspace_image()} adds an image annotation layer to a
#' \code{ggplot}-based \code{GraphSpace} plot.
#'
#' @param raster An image to be displayed. Accepted types:
#'   \itemize{
#'     \item A \code{\link{GraphSpace}} object — the image is extracted via
#'       \code{\link{gs_image}}.
#'     \item A \code{raster} object
#'       (see \code{\link[grDevices]{as.raster}}).
#'     \item A \code{matrix} or 3D array (RGB/RGBA), coerced to
#'       \code{raster} automatically.
#'   }
#' @param interpolate A logical value indicating whether to apply linear
#'   interpolation when the image is rendered at a different resolution than
#'   its native size. Defaults to \code{FALSE}.
#' @param opacity A numeric value in \code{[0, 1]} controlling the
#'   transparency of the image. \code{1} is fully opaque (default);
#'   \code{0} is fully transparent.
#' @param flip.v A logical value; if \code{TRUE}, the image is flipped
#'   vertically (top-to-bottom). Defaults to \code{FALSE}.
#' @param flip.h A logical value; if \code{TRUE}, the image is flipped
#'   horizontally (left-to-right). Defaults to \code{FALSE}.
#' @param ... Additional arguments (currently unused).
#' 
#' @return A ggplot2 layer object that can be added to a \code{ggplot()}
#'   call with \code{+}, or \code{invisible(NULL)} with a warning if the
#'   image could not be resolved.
#'
#' @seealso
#' \code{\link[ggplot2]{annotation_raster}},
#' \code{\link{gs_image}},
#' \code{\link{geom_nodespace}},
#' \code{\link{geom_edgespace}}
#'
#' @examples
#' 
#' # Assuming 'gs' is a GraphSpace object with 
#' # an image stored in gs_image(gs)
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
annotation_gspace_image <- function(raster, interpolate = FALSE, opacity = 1,
  flip.v = FALSE, flip.h = FALSE) {

  if (missing(raster)) {
    rlang::abort("Argument 'raster' is missing, with no default.")
  }
  
  .validate_gs_args("singleLogical", "interpolate", interpolate)
  .validate_gs_args("singleLogical", "flip.v", flip.v)
  .validate_gs_args("singleLogical", "flip.h", flip.h)
  .validate_gs_args("singleNumber", "opacity", opacity)
  
  if (inherits(raster, "GraphSpace")) {
    if (!.has_image(raster)) {
      rlang::warn("The 'GraphSpace' object contains no image.")
      return(invisible(NULL))
    }
    raster <- gs_image(raster)
  }
  
  if (!inherits(raster, "raster")) {
    raster <- tryCatch({
      grDevices::as.raster(raster)
    }, error = function(e) {
      rlang::warn(c(
        "x" = "Failed to convert 'raster' to a valid raster object.",
        "i" = "Accepted types: matrix, array (RGB/RGBA), or raster."
      ))
      NULL
    })
    if (is.null(raster)) return(invisible(NULL))
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
    img <- grDevices::col2rgb(as.character(raster), alpha = TRUE)
    img[4, ] <- as.integer(opacity * 255)
    raster <- as.raster(matrix(
      grDevices::rgb(img[1,], img[2,], img[3,], img[4,], maxColorValue = 255),
      nrow = nrow(raster), ncol = ncol(raster),
      byrow = TRUE
    ))
  }

  if (flip.v) raster <- raster[rev(seq_len(nrow(raster))), , drop = FALSE]
  if (flip.h) raster <- raster[, rev(seq_len(ncol(raster))), drop = FALSE]

  ggplot2::annotation_raster(
    raster      = raster,
    xmin        = 0,
    xmax        = 1,
    ymin        = 0,
    ymax        = 1,
    interpolate = interpolate
  )
}

#' @note \code{annotation_gspace()} is deprecated as of v1.4.0; use 
#' \code{annotation_gspace_image()} instead.
#' @rdname annotation_gspace_image
#' @export
annotation_gspace <- function(...) {
  
  lifecycle::deprecate_warn(
    when = "1.4.0",
    what = "annotation_gspace()",
    with = "annotation_gspace_image()"
  )
  
  annotation_gspace_image(...)
  
}
