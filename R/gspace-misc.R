
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
  
  z <- scales::rescale(x, to = c(1, length(palette)))
  z <- pmin(pmax(round(z), 1), length(palette))
  
  m <- palette[z]
  m[is.na(x)] <- na.color
  
  if (is.matrix(x)) {
    dim(m) <- dim(x)
  }
  
  as.raster(m)

}


