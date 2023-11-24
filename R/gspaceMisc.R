
################################################################################
### Package documentation
################################################################################
#' @keywords internal
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
#' \tab Plotting igraph objects with RGraphSpace package.\cr
#' \link{getGraphSpace}:
#' \tab Accessors for fetching slots from a GraphSpace object.\cr
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
"_PACKAGE"
#> [1] '_PACKAGE'

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


