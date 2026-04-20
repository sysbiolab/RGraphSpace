
#-------------------------------------------------------------------------------
#' Normalize node coordinates to graph and image spaces
#' 
#' Accessory functions to normalize node coordinates in GraphSpace, 
#' either by centering them within the graph boundaries 
#' or by mapping them to pixel coordinates of a background image.
#' 
#' @param gs A \code{GraphSpace} object to be normalized.
#' @param image An optional background image. When provided, \code{x} and 
#' \code{y} coordinates must represent pixel positions in the image matrix.
#' @param ... Additional arguments passed to specific normalization workflows.
#' @param mar A single numeric value in \code{[0, 0.5]} controlling the size of
#' the outer margins around the graph. Without an image, \code{mar} specifies
#' symmetric margins as a fraction of the graph space. With an image,
#' \code{mar} is interpreted as a fraction of the available image margins
#' surrounding the graph.
#' @param flip.y Logical; whether to flip the node coordinates along the y-axis.
#' @param flip.x Logical; whether to flip the node coordinates along the x-axis.
#' @param rotate.xy Logical; whether to rotate x-y coordinates.
#' @param flip.v Logical; whether to vertically flip the background image  
#' matrix (top-to-bottom) to align with the graph coordinate system.
#' @param flip.h Logical; whether to horizontally flip the background image  
#' matrix (left-to-right) to align with the graph coordinate system.
#' @param crop.coord An optional numeric vector of length four specifying a  
#' cropping region (xmin, xmax, ymin, ymax), with values in normalized 
#' coordinates \code{[0,1]}.
#' @param verbose A single logical value specifying to display detailed 
#' messages (when \code{verbose=TRUE}) or not (when \code{verbose=FALSE}).
#' 
#' @usage
#' normalizeGraphSpace(gs, image = NULL, ...)
#' 
#' @details 
#' These functions provide different strategies for coordinate transformation:
#' \itemize{
#'   \item \bold{normalizeGraphSpace}: Rescales node coordinates to a \code{[0, 1]} 
#'   unit square based on the graph's bounding box (when \code{image} is missing) 
#'   or maps them to pixel coordinates (when \code{image} is provided). It handles 
#'   image-to-graph alignment via \code{flip.v}, \code{flip.h}, \code{flip.x}, 
#'   \code{flip.y}, and \code{rotate.xy}, used to adjust the graph origin 
#'   with the image matrix layout.
#' 
#'   \item \bold{cropGraphSpace}: Subsets the normalized graph space into a 
#'   specific region defined by \code{crop.coord}. It recalculates node positions 
#'   and background image boundaries to maintain spatial consistency after cropping. 
#'   This function requires a previously normalized \code{GraphSpace} object.
#' }
#' 
#' @return A \code{GraphSpace} object with updated \code{nodes} 
#' and \code{image} slots.
#' 
#' @note This is an accessory function typically called during 
#' the preprocessing of \code{GraphSpace} objects before rendering.
#'
#' @examples
#' library(igraph)
#' 
#' # Create a star graph
#' gtoy1 <- make_full_graph(15)
#' 
#' # Create a GraphSpace
#' gs <- GraphSpace(gtoy1)
#' 
#' gs <- normalizeGraphSpace(gs)
#' 
#' @aliases normalizeGraphSpace
#' @rdname normalizeGraphSpace-methods
#' @export
setMethod("normalizeGraphSpace",
  signature = c(gs = "GraphSpace", image = "missing"),
  function(gs, image, ...,  mar = 0.1, flip.x = FALSE, flip.y = FALSE, 
    rotate.xy = FALSE, crop_method = "", verbose = TRUE){
    
    if(gs@pars$image.layer) {
      gs@pars$image.layer <- FALSE
    }
    
    extra_args <- list(...)
    if(length(extra_args) > 0 && verbose) {
      warning("Arguments ignored: ", paste(names(extra_args), collapse = ", "))
    }
    
    .validate_gs_args("singleLogical", "flip.x", flip.x)
    .validate_gs_args("singleLogical", "flip.y", flip.y)
    .validate_gs_args("singleLogical", "rotate.xy", rotate.xy)
    .validate_gs_args("singleLogical", "verbose", verbose)
    .validate_gs_args("singleNumber", "mar", mar)
    
    if (mar < 0 || mar > 0.5) {
      warning("'mar' should be in [0, 0.5]", call. = FALSE)
      mar <- max(0, min(mar, 0.5))
    }
    
    nodes <- .get_nodes(gs@graph)
    if(nrow(nodes)>0){
      nodes <- .setCoordToGraph(nodes, flip.x, flip.y, rotate.xy, verbose)
      if(verbose) message("Normalizing node coordinates to graph space...")
      gs@nodes <- .fit_graph_space(nodes, mar)
      gs@pars$is.normalized <- TRUE
      gs@pars$flip.x <- flip.x
      gs@pars$flip.y <- flip.y
      gs@pars$rotate.xy <- rotate.xy
      gs@pars$mar <- mar
    }
    
  return(gs)
    
})

#-------------------------------------------------------------------------------
#' @aliases normalizeGraphSpace
#' @rdname normalizeGraphSpace-methods
#' @export
setMethod("normalizeGraphSpace", 
  signature = c(gs = "GraphSpace", image = "ANY"),
  function(gs, image, ..., mar = 0.1, flip.x = FALSE, flip.y = FALSE, 
    rotate.xy = FALSE, flip.v = TRUE, flip.h = FALSE, verbose = TRUE){
    
    if(is.null(image)) {
      return(normalizeGraphSpace(gs = gs, mar = mar, 
        flip.x = flip.x, flip.y = flip.y, 
        rotate.xy = rotate.xy, verbose = verbose))
    }
    
    extra_args <- list(...)
    if(length(extra_args) > 0 && verbose) {
      warning("Arguments ignored: ", paste(names(extra_args), collapse = ", "))
    }
    
    .validate_gs_args("image_mtx", "image", image)
    .validate_gs_args("singleLogical", "flip.v", flip.v)
    .validate_gs_args("singleLogical", "flip.h", flip.h)
    .validate_gs_args("singleLogical", "flip.x", flip.x)
    .validate_gs_args("singleLogical", "flip.y", flip.y)
    .validate_gs_args("singleLogical", "rotate.xy", rotate.xy)
    .validate_gs_args("singleLogical", "verbose", verbose)
    .validate_gs_args("singleNumber", "mar", mar)
    
    if (mar < 0 || mar > 0.5) {
      warning("'mar' should be in [0, 0.5]", call. = FALSE)
      mar <- max(0, min(mar, 0.5))
    }
    
    nodes <- .get_nodes(gs@graph)
    if(nrow(nodes)>0){
      if(verbose) message("Normalizing node coordinates to image space...")
      image_adj <- image
      if(!is.raster(image_adj)) image_adj <- as.raster(image_adj)
      if(flip.v){
        if(verbose) message("Flipping image top-to-bottom...")  
        image_adj <- image_adj[rev(seq_len(nrow(image_adj))), , drop = FALSE]
      } 
      if(flip.h){
        if(verbose) message("Flipping image left-to-right...")  
        image_adj <- image_adj[, rev(seq_len(ncol(image_adj))), drop = FALSE]
      } 
      nodes <- .setCoordToImage(nodes, image_adj, flip.x, flip.y,
        rotate.xy, verbose)
      l_temp <- .fitImageNodes(nodes, image_adj, mar)
      gs@image <- l_temp$image
      gs@nodes <- l_temp$nodes
      gs@pars$is.normalized <- TRUE
      gs@pars$image.layer <- TRUE
      gs@pars$flip.v <- flip.v
      gs@pars$flip.x <- flip.x
      gs@pars$flip.y <- flip.y
      gs@pars$rotate.xy <- rotate.xy
      gs@pars$mar <- mar
      gs@misc$image <- image
    }
    
    return(gs)
    
  })

#-------------------------------------------------------------------------------
#' @aliases cropGraphSpace
#' @rdname normalizeGraphSpace-methods
#' @export
setMethod("cropGraphSpace", "GraphSpace", 
  function(gs, crop.coord = c(0, 1, 0, 1), verbose = TRUE){
    .validate_gs_args("numeric_vec", "crop.coord", crop.coord)
    if(length(crop.coord)!=4){
      stop("'crop.coord' should be a numeric vector of length = 4.", 
        call. = FALSE)
    }
    if(any(crop.coord < 0) || any(crop.coord > 1)){
      stop("'crop.coord' should be in [0,1].", call. = FALSE)
    }
    if(!gs@pars$is.normalized){
      stop("'GraphSpace' must be normalized.", call. = FALSE)
    }
    
    gs <- .crop_gspace(gs, crop.coord)
    
    return(gs)
    
  })

################################################################################
### Graph adjusts
################################################################################
.setCoordToGraph <- function(nodes, flip.x = FALSE, flip.y = FALSE, 
  rotate.xy= FALSE, verbose = TRUE){
  
  # Rotated coordinates
  coord <- nodes[,c("x","y")]
  if(rotate.xy){
    if(verbose) message("Rotating xy-coordinates...")
    coord$x2 <- coord$y
    coord$y2 <- coord$x
  } else {
    coord$x2 <- coord$x
    coord$y2 <- coord$y
  }
  
  # Flip y-coordinates
  if(flip.y){
    if(verbose) message("Flipping y-coordinates...")
    y <- coord$y2
    coord$y2 <- -(y - max(y)) - max(y) + 1
  }
  
  # Flip x-coordinates
  if(flip.x){
    if(verbose) message("Flipping x-coordinates...")
    x <- coord$x2
    coord$x2 <- -(x - max(x)) - max(x) + 1
  }
  # Update coordinates
  nodes$x <- coord$x2
  nodes$y <- coord$y2
  
  return(nodes)
}

################################################################################
### Adjust node coordinates
################################################################################
# Fit graph in a [0, 1] space with focus on adjusting margins
.fit_graph_space <- function(nodes, mar = 0.1){
  
  mar <- max(0, min(mar, 0.49))
  
  nds <- nodes
  
  if(nrow(nds)>0){
    
    nds$x <- nds$x - mean(range(nds$x))
    nds$y <- nds$y - mean(range(nds$y))
    
    from <- range(c(nds$x, nds$y))
    to <- c(mar, 1 - mar)
    
    nds$x <- scales::rescale(nds$x, from = from, to=to)
    nds$y <- scales::rescale(nds$y, from = from, to=to)
    
  }
  
  return(nds)
}

################################################################################
### Graph-to-image adjusts
################################################################################
.setCoordToImage <- function(nodes, image, 
  flip.x = FALSE, flip.y = FALSE, rotate.xy = FALSE, 
  verbose = TRUE){
  
  # Rotated coordinates
  coord <- nodes[,c("x","y")]
  if(rotate.xy){
    if(verbose) message("Rotating xy-coordinates...")
    coord$x2 <- coord$y
    coord$y2 <- coord$x
  } else {
    coord$x2 <- coord$x
    coord$y2 <- coord$y
  }
  
  # Flip y-coordinates over image axis
  if(flip.y){
    if(verbose) message("Flipping y-coordinates...")
    y <- coord$y2
    y <- -(y - max(y)) + nrow(image) - max(y) + 1
    rg <- range(y)
    if(min(rg)<1 || max(rg)>nrow(image)){
      ms2 <- "Revise normalizeImageSpace() arguments."
      if(rotate.xy){
        ms1 <- "Graph rotation/flip incompatible with the input image dimensions. "
      } else {
        ms1 <- "Graph flipping incompatible with the input image dimensions. "
      }
      stop(paste0(ms1, ms2), call. = FALSE)
    }
    coord$y2 <- y
  }
  
  # Flip x-coordinates over image axis
  if(flip.x){
    if(verbose) message("Flipping x-coordinates...")
    x <- coord$x2
    x <- -(x - max(x)) + ncol(image) - max(x) + 1
    rg <- range(x)
    if(min(rg)<1 || max(rg)>ncol(image)){
      ms2 <- "Revise buildSpotSpace() arguments."
      if(rotate.xy){
        ms1 <- "Graph rotation/flip not compatible with the input image dimensions. "
      } else {
        ms1 <- "Graph flipping not compatible with the input image dimensions. "
      }
      stop(paste0(ms1, ms2), call. = FALSE)
    }
    coord$x2 <- x
  }
  
  # Update coordinates
  nodes$x <- coord$x2
  nodes$y <- coord$y2
  
  return(nodes)
  
}

################################################################################
### Adjust image to node coordinates
################################################################################
.fitImageNodes <- function(nodes, image, mar = 0.1){
  d <- dim(image)
  xr <- range(nodes$x)
  yr <- range(nodes$y)
  if( (xr[1] < 1) || (xr[2] > d[2]) ){
    stop("Graph coordinates outside image dimensions.", call. = FALSE)
  }
  if( (yr[1] < 1) || (yr[2] > d[1]) ){
    stop("Graph coordinates outside image dimensions.", call. = FALSE)
  }
  l_temp <- .fit_image_nodes(nodes, image, mar)
  l_temp <- .adjust_aspect_ratio(l_temp)
  l_temp <- .normalize_image_nodes(l_temp)
  return(l_temp)
}

#-------------------------------------------------------------------------------
# Fit image to nodes with focus on adjusting graph margins
.fit_image_nodes <- function(nodes, image, mar = 0.1) {
  
  nds <- nodes
  img <- image
  d <- dim(img)
  mar <- max(0, min(mar, 0.49))
  
  # bounding box around nodes
  xl_nds <- range(nds$x)
  yl_nds <- range(nds$y)
  center_x <- mean(xl_nds)
  center_y <- mean(yl_nds)
  
  # target dimension centered on nodes;
  # side_length is calculated so that max_d is 
  # exactly (1 - 2*mar) of the total width
  max_d <- max(diff(xl_nds), diff(yl_nds))
  side_length <- max_d / (1 - 2 * mar)
  half_side <- side_length / 2
  
  # initial crop coordinates
  x_start <- center_x - half_side
  x_end   <- x_start + side_length
  y_start <- center_y - half_side
  y_end   <- y_start + side_length
  
  # shift crop coordinates to the image boundaries
  if (x_start < 1) { 
    shift <- 1 - x_start
    x_start <- 1
    x_end <- min(d[2], x_end + shift)
  }
  if (x_end > d[2]) { 
    shift <- x_end - d[2]
    x_end <- d[2]
    x_start <- max(1, x_start - shift)
  }
  if (y_start < 1) { 
    shift <- 1 - y_start
    y_start <- 1
    y_end <- min(d[1], y_end + shift)
  }
  if (y_end > d[1]) { 
    shift <- y_end - d[1]
    y_end <- d[1]
    y_start <- max(1, y_start - shift)
  }
  
  # force the limits to include the node bounding box
  x_start <- max(1, min(x_start, xl_nds[1]))
  x_end   <- min(d[2], max(x_end, xl_nds[2]))
  y_start <- max(1, min(y_start, yl_nds[1]))
  y_end   <- min(d[1], max(y_end, yl_nds[2]))
  
  # convert to indices
  x_s_idx <- floor(x_start)
  x_e_idx   <- ceiling(x_end)
  y_s_idx <- floor(y_start)
  y_e_idx   <- ceiling(y_end)
  
  # final validity check
  x_s_idx <- max(1L, x_s_idx)
  x_e_idx   <- min(d[2], x_e_idx)
  y_s_idx <- max(1L, y_s_idx)
  y_e_idx   <- min(d[1], y_e_idx)
  
  # execute crop on flipped image
  img_res <- img[seq.int(d[1], 1), ]
  img_res <- img_res[seq.int(y_s_idx, y_e_idx), seq.int(x_s_idx, x_e_idx)]
  img_res <- img_res[seq.int(nrow(img_res), 1), ]
  
  # update node coordinates
  nds$x <- nds$x - x_s_idx + 1
  nds$y <- nds$y - y_s_idx + 1
  
  # calculate final side_length
  x_side_length <- x_end - x_start
  y_side_length <- y_end - y_start
  x_side_length <- x_side_length + (x_start - x_s_idx)
  y_side_length <- y_side_length + (y_start - y_s_idx)
  side_length <- max(x_side_length, y_side_length)
  
  return(list(nodes = nds, image = img_res, 
    side_length = side_length))
}

#-------------------------------------------------------------------------------
.adjust_aspect_ratio <- function(l_temp){
  d <- dim(l_temp$image)
  if(d[1] > d[2]){
    n <- ceiling( (d[1] - d[2]) )/2
    img_d <- matrix(NA, nrow = d[1], ncol = d[1])
    img_d[ , seq(n + 1, n + d[2])] <- as.matrix(l_temp$image)
    l_temp$nodes$x <- l_temp$nodes$x + n
    l_temp$image  <- as.raster(img_d)
  } else if(d[1] < d[2]){
    n <- ceiling( (d[2] - d[1])/2 )
    img_d <- matrix(NA, nrow = d[2], ncol = d[2])
    img_d[seq(n + 1, n + d[1]), ] <- as.matrix(l_temp$image)
    l_temp$nodes$y <- l_temp$nodes$y + n
    l_temp$image  <- as.raster(img_d)
  }
  return(l_temp)
}

#-------------------------------------------------------------------------------
.normalize_image_nodes <- function(l_temp){
  d <- dim(l_temp$image)
  l_temp$nodes$x <- .rescale_direct(l_temp$nodes$x, d[2], 0.5 / d[2])
  l_temp$nodes$y <- .rescale_direct(l_temp$nodes$y, d[1], 0.5 / d[1])
  return(l_temp)
}
.rescale_direct <- function(x, n, half_pixel) {
  ((x - 1) / (n - 1)) * (1 - 2 * half_pixel) + half_pixel
}

################################################################################
### Crop graph and image
################################################################################
.crop_gspace <- function(gs, crop.coord){
  
  # remove nodes
  nodes <- gs@nodes
  cx <- nodes$x >= crop.coord[1] & nodes$x <= crop.coord[2]
  cy <- nodes$y >= crop.coord[3] & nodes$y <= crop.coord[4]
  nodes <- nodes[ which(cx & cy), ]
  
  # remove edges
  idx <- (gs@edges$name1 %in% gs@nodes$name) & 
    (gs@edges$name2 %in% gs@nodes$name) 
  gs@edges <- gs@edges[idx,]
  
  # center nodes
  nodes$x <- nodes$x - mean(range(nodes$x))
  nodes$y <- nodes$y - mean(range(nodes$y))
  from <- range(c(nodes$x, nodes$y))
  nodes$x <- scales::rescale(nodes$x, from = from, to=c(0,1))
  nodes$y <- scales::rescale(nodes$y, from = from, to=c(0,1))
  
  # update graph
  idx <- V(gs@graph)$name %in% rownames(nodes)
  gs@graph <- igraph::delete_vertices(gs@graph, which(!idx))
  idx <- match(rownames(nodes), V(gs@graph)$name)
  V(gs@graph)$x[idx] <- nodes$x
  V(gs@graph)$y[idx] <- nodes$y
  gs@nodes <- nodes
  
  # crop image
  if(gs@pars$image.layer){
    nrow_mat <- nrow(gs@image)
    ncol_mat <- ncol(gs@image)
    xmin <- max(1, floor(crop.coord[1] * ncol_mat) + 1)
    xmax <- min(ncol_mat, ceiling(crop.coord[2] * ncol_mat))
    ymin <- max(1, floor(crop.coord[3] * nrow_mat) + 1)
    ymax <- min(nrow_mat, ceiling(crop.coord[4] * nrow_mat))
    gs@image <- gs@image[ymin:ymax, xmin:xmax, drop = FALSE]
  }
  
  return(gs)
}

