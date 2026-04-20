
################################################################################
### Package documentation
################################################################################
#' @aliases RGraphSpace-package
#' @title RGraphSpace: A lightweight package for representing large igraph 
#' objects in a normalized coordinate system
#'
#' @description
#' RGraphSpace is an R package that integrates igraph and ggplot2 graphics 
#' within spatial maps. RGraphSpace implements new geometric objects using 
#' ggplot2 protypes, customized for representing large igraph objects in a 
#' normalized coordinate system. By scaling shapes and graph elements, 
#' RGraphSpace can provide a framework for layered visualizations.
#'
#' @details
#'
#' \tabular{ll}{
#' Package: \tab RGraphSpace\cr
#' Type: \tab Software\cr
#' License: \tab GPL-3\cr
#' Maintainer: \tab Mauro Castro \email{mauro.a.castro@@gmail.com}\cr
#' }
#'
#' @section Index:
#' \tabular{ll}{
#' \link{GraphSpace}:
#' \tab Constructor of GraphSpace-class objects.\cr
#' \link{plotGraphSpace}:
#' \tab Wrapper function to plot GraphSpace objects in ggplot2.
#' }
#' Further information is available in the vignettes by typing
#' \code{vignette('RGraphSpace')}. Documented topics are also available in
#' HTML by typing \code{help.start()} and selecting the RGraphSpace package
#' from the menu.
#'
#' @references
#' Castro MAA, Wang X, Fletcher MNC, Meyer KB, Markowetz F. RedeR:
#' R/Bioconductor package for representing modular structures, nested
#' networks and multiple levels of hierarchical associations.
#' Genome Biology 13:R29, 2012.
#' 
#' @importFrom lifecycle deprecated deprecate_soft is_present
#' @keywords internal
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
#' @usage data(gtoy1)
#'
#' @source This package.
#'
#' @docType data
#' @keywords gtoys
#' @name gtoys
#' @aliases gtoy1 gtoy2
#' @return A pre-processed igraph object.
#' @examples
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
#'   \code{viridisLite::viridis(256)} is used.
#' @param na.color Color used for \code{NA} values. Defaults to \code{NA}.
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
#' # Convert the volcano matrix to a color raster
#' img <- as_colorraster(volcano)
#' plot(img)
#'
#' # Use a different color palette
#' img <- as_colorraster(volcano, palette = terrain.colors(256))
#' plot(img)
#'
#' @importFrom grDevices hcl.colors
#' @export
as_colorraster <- function(x, palette = hcl.colors(256), na.color = "white") {
  
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


