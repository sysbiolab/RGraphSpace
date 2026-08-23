#-------------------------------------------------------------------------------
#' Normalize or fit node geometry
#'
#' Two related operations for keeping an \code{sfc} geometry column
#' attached to a \code{GraphSpace}'s nodes in registration with the node
#' coordinates, for two different situations.
#'
#' @param gs A \code{GraphSpace} object.
#' @param name Character. Name of the geometry column to operate on.
#' @param use_node_size Logical. If \code{TRUE} (the default),
#' \code{fitGeometry()} also rescales each geometry to match its node's
#' \code{nodeSize}. If \code{FALSE}, only repositioning happens, each feature 
#' keeps its current size.
#' @param verbose Logical. Whether to report progress messages.
#'
#' @return The updated \code{GraphSpace} object.
#'
#' @details
#' **\code{normalizeGeometry}** is for geometry that is already spatially
#' meaningful, with its own coordinates genuinely correspond to the nodes
#' (e.g. real cell-segmentation boundaries) and only needs realigning
#' to the current, normalized node frame. It fits a linear regression
#' between the geometry's centroids and the node coordinates and rescales
#' the geometry accordingly, warning if the fit is poor (the geometry did
#' not, in fact, scale linearly with the nodes).
#'
#' **\code{fitGeometry}** is for geometry that is not yet spatially
#' related to the nodes, as arbitrary shapes used for node markers. It
#' repositions every shape so its centroid sits exactly at its node's
#' coordinates and, when \code{use_node_size = TRUE}, also rescales
#' each shape so its diameter matches \code{nodeSize}.
#'
#' Both require \code{gs} to already be normalized (see
#' \code{\link{normalizeGraphSpace}}), and both operate on a single named
#' geometry column, leaving any other geometry columns untouched.
#' 
#'
#' @examples
#' \dontrun{
#' # Mode 1: geometry already spatially meaningful, just needs realigning
#' gs_geometry(gs, "geometry") <- real_cell_boundaries
#' gs <- normalizeGeometry(gs)
#'
#' # Mode 2: arbitrary shapes, sized and positioned like nodes
#' gs_geometry(gs, "geometry") <- arbitrary_shapes
#' gs <- fitGeometry(gs, use_node_size = TRUE)
#' }
#' 
#' @aliases normalizeGeometry
#' @rdname geometry-methods
#' @export
setMethod("normalizeGeometry", "GraphSpace",
  function(gs,  name = "geometry", verbose = TRUE){
    
    gs <- updateGraphSpace(gs)
    
    .validate_gs_args("singleString", "name", name)
    .validate_gs_args("singleLogical", "verbose", verbose)
    
    if(!.is_normalized(gs)){
      rlang::abort(
        message = c(
          "The 'GraphSpace' object must be normalized before normalizing geometry.",
          "i" = "Please run 'normalizeGraphSpace(gs)' first."
        )
      )
    }
    
    valid_names <- .gs_geometry_cols(getGraphSpace(gs, "coords"))
    
    if(name %in% valid_names){
      gs <- .gs_geometry_normalize(gs, name = name, verbose = verbose)
    } else {
      rlang::warn(sprintf("Name '%s' not a valid geometry in the `gs` object", name))
    }
    
    return(gs)
  }
)

#-------------------------------------------------------------------------------
#' @aliases fitGeometry
#' @rdname geometry-methods
#' @export
setMethod("fitGeometry", "GraphSpace",
  function(gs, name = "geometry", use_node_size = TRUE, verbose = TRUE){
    
    gs <- updateGraphSpace(gs)
    
    .validate_gs_args("singleString", "name", name)
    .validate_gs_args("singleLogical", "use_node_size", use_node_size)
    .validate_gs_args("singleLogical", "verbose", verbose)
    
    valid_names <- .gs_geometry_cols(getGraphSpace(gs, "nodes"))
    
    if(name %in% valid_names){
      gs <- .gs_fit_geometry(gs, name, use_node_size, verbose)
    } else {
      rlang::warn(sprintf("Name '%s' not a valid geometry in the `gs` object", name))
    }
    
    return(gs)
  }
)

#-------------------------------------------------------------------------------
#' @keywords internal
.gs_geometry_normalize <- function(x, name = "geometry", 
  r2_threshold = 0.99, verbose = TRUE) {
  
  geometry <- x@coords[[name]]
  
  if (!inherits(geometry, "sfc")) {
    rlang::abort(sprintf("`%s` is not a geometry (sfc) column.", name))
  }
  
  centers <- sf::st_coordinates(sf::st_centroid(geometry))
  
  fit_x <- lm(x$x ~ centers[, "X"])
  fit_y <- lm(x$y ~ centers[, "Y"])
  
  r_squared <- function(fit) {
    y <- fit$model[[1]]
    1 - sum(residuals(fit)^2) / sum((y - mean(y))^2)
  }
  r2_x <- r_squared(fit_x)
  r2_y <- r_squared(fit_y)
  
  if (verbose) rlang::inform(sprintf("Normalizing '%s' coordinates...", name))
  if (r2_x < r2_threshold || r2_y < r2_threshold) {
    rlang::warn( message = c(
      sprintf("'%s' did not scale linearly with node coordinates.", name),
      "i" = sprintf("R-squared: x = %.5f, y = %.5f (threshold = %.5f).",
        r2_x, r2_y, r2_threshold),
      "i" = sprintf("Alignment between '%s' and the normalized nodes may be inaccurate.", name)
    )
    )
  }
  
  bx <- coef(fit_x)[[2]]; ax <- coef(fit_x)[[1]]
  by <- coef(fit_y)[[2]]; ay <- coef(fit_y)[[1]]
  
  M <- matrix(c(bx, 0, 0,  by), nrow = 2, byrow = TRUE)
  geometry <- geometry * M + c(ax, ay)
  geometry <- sf::st_set_crs(geometry, NA)
  
  x@nodes[[name]] <- geometry
  
  x
}

#-------------------------------------------------------------------------------
.gs_fit_geometry <- function(x, name, use_node_size = TRUE, verbose = TRUE) {

  if (use_node_size && anyNA(x$nodeSize)) {
    rlang::abort("nodeSize contains NA; every node needs a size to fit geometry to.")
  }
  
  geom <- x@nodes[[name]]
  
  if (use_node_size) {
    if (verbose){
      rlang::inform(sprintf("Fitting '%s' geometry to node size...", name))
    }
    npc_per_unit <- .gs_nsz_to_npc() # same constant geom_nodespace() itself uses
    target_diam <- x$nodeSize * npc_per_unit
    current_diam <- .geometry_diameter(geom)
    
    zero_extent <- current_diam < 1e-9
    if (any(zero_extent)) {
      geom[zero_extent] <- sf::st_buffer(geom[zero_extent], 
        dist = target_diam[zero_extent] / 2)
    }
    
    scaled <- geom
    if (any(!zero_extent)) {
      centroids_s <- sf::st_centroid(geom[!zero_extent])
      scale_vec <- target_diam[!zero_extent] / current_diam[!zero_extent]
      scaled[!zero_extent] <- (sf::st_geometry(geom[!zero_extent]) - 
          centroids_s) * scale_vec + centroids_s
    }
  } else {
    scaled <- geom
  }
  
  if (verbose){
    rlang::inform(sprintf("Fitting '%s' geometry to node coordinates...", name))
  }
  centroids <- sf::st_centroid(scaled)
  
  # nodes
  targets <- sf::st_cast(sf::st_sfc(sf::st_multipoint(as.matrix(x@nodes[, c("x","y")]))), "POINT")
  geom_fixed <- (sf::st_geometry(scaled) - centroids) + targets
  x <- .add_node_geometry(x, name, geom_fixed, slots = "nodes", verbose = FALSE)

  x
  
}

#-------------------------------------------------------------------------------
# Max pairwise distance between any two vertices of each feature,
# the exact geometric diameter, 0 for a bare POINT.
.geometry_diameter <- function(geom) {
  circles <- sf::st_minimum_bounding_circle(geom)
  areas <- sf::st_area(circles)
  2 * sqrt(areas / pi)   # area = pi*r^2, so diameter = 2*sqrt(area/pi)
}

#-------------------------------------------------------------------------------
# Recursively pulls coordinate matrices out of any sfg structure,
# regardless of type or nesting depth (POINT, POLYGON, etc).
.extract_sfg_coords <- function(g) {
  if (is.matrix(g)) return(list(g[, 1:2, drop = FALSE]))
  if (is.list(g)) return(do.call(c, lapply(g, .extract_sfg_coords)))
  if (is.numeric(g) && length(g) >= 2) return(list(matrix(g[1:2], nrow = 1)))
  list()
}

#-------------------------------------------------------------------------------
#' @keywords internal
.add_node_geometry <- function(x, name, value, 
  slots = c("coords&nodes","coords","nodes"), verbose = TRUE) {
  
  slots <- match.arg(slots)
  
  if (inherits(value, "sf")) {
    value <- sf::st_geometry(value)
  } else if (is.list(value) && length(value) > 0 ) {
    bl <- vapply(value, inherits, logical(1), what = "sfg")
    if(all(bl)){
      value <- sf::st_sfc(value)
    }
  }
  
  if (!inherits(value, "sfc")) {
    rlang::abort(sprintf(
      "'value' must be an 'sfc' geometry column, not '%s'.",
      paste(class(value), collapse = "/")))
  }
  
  if(length(value) != gs_vcount(x)){
    rlang::abort(sprintf(
      "'value' has %d geometries but 'x' has %d nodes; lengths must match.",
      length(value), gs_vcount(x)))
  }
  
  valid <- sf::st_is_valid(value)
  n_bad <- sum(!valid | is.na(valid))
  if (n_bad > 0) {
    rlang::warn(
      message = c(
        sprintf("'%s' contains invalid geometries.", name),
        "i" = sprintf("%d of %d geometries failed 'sf::st_is_valid()'.", n_bad, length(valid)),
        "i" = "Downstream operations (plotting, transforms) may error or behave unexpectedly.",
        "*" = sprintf("Consider 'sf::st_make_valid(%s)' before assigning.", name)
      )
    )
  }
  
  if(slots == "coords&nodes"){
    x@coords[[name]] <- value
    x@nodes[[name]] <- value 
  } else if(slots == "coords"){
    x@coords[[name]] <- value
  } else {
    x@nodes[[name]] <- value 
  }
  
  x
}

#-------------------------------------------------------------------------------
#' @keywords internal
.is_valid_geometry <- function(value){
  c1 <- inherits(value, "sfc") || inherits(value, "sf")
  c2 <- (is.list(value) && length(value) > 0 && all(vapply(value, inherits, logical(1), "sfg")))
  c1 || c2
}


#-------------------------------------------------------------------------------
# gs_geometry_size <- function(gs, name = "geometry", size = NA, verbose = TRUE) {
#   
#   gs <- updateGraphSpace(gs)
#   
#   if(!.is_normalized(gs)){
#     rlang::abort(
#       message = c(
#         "The 'GraphSpace' object must be normalized before fitting geometry.",
#         "i" = "Please run 'normalizeGraphSpace(gs)' first."
#       )
#     )
#   }
#   
#   if (anyNA(gs$nodeSize)) {
#     rlang::abort("'nodeSize' contains NA; every node needs a size to fit geometry to.")
#   }
#   
#   geom <- gs@coords[[name]]
#   
#   if (verbose){
#     rlang::inform(sprintf("Fitting '%s' geometry to node size...", name))
#   }
#   npc_per_unit <- .gs_nsz_to_npc()     # same constant geom_nodespace() itself uses
#   target_diam <- gs$nodeSize * npc_per_unit
#   current_diam <- .geometry_diameter(geom)
#   
#   zero_extent <- current_diam < 1e-9
#   if (any(zero_extent)) {
#     geom[zero_extent] <- sf::st_buffer(geom[zero_extent], dist = target_diam[zero_extent] / 2)
#   }
#   
#   scaled <- geom
#   if (any(!zero_extent)) {
#     centroids_s <- sf::st_centroid(geom[!zero_extent])
#     scale_vec <- target_diam[!zero_extent] / current_diam[!zero_extent]
#     scaled[!zero_extent] <- (sf::st_geometry(geom[!zero_extent]) - centroids_s) * scale_vec + centroids_s
#   }
#   
#   gs <- .add_node_geometry(gs, name, scaled, slots = "nodes", verbose = FALSE)
#   
#   gs
# }

