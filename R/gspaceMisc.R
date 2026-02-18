
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


#-------------------------------------------------------------------------------
#' @title Create standalone legends for RGraphSpace's ggplot objects
#'
#' @description
#' This accessor builds a standalone legend from a named vector of colors. The 
#' resulting legend is returned as a 'grob' object that can be used to add 
#' external legends to existing ggplots.
#'
#' @param color_palette A named character vector of colors. Names become 
#' legend labels. Values must be valid R color specifications, e.g. 
#' `color_palette = c(A = "#FF0000", B = "blue")`.
#' @param breaks A vector indicating numeric breaks for a continuous legend. 
#' If missing, `color_palette` will instead produce a discrete legend.
#' @param legend_title The legend title.
#' @param legend_shape Point shape (default 21, for circle).
#' @param legend_size Size of legend points (default 6 mm).
#' @param text_size Text size (default 10 mm).
#' @param orientation Legend arrangement ("vertical" or "horizontal").
#' @param justification Legend justification within bounding box 
#' ("left" or "right").
#' @param plot A GraphSpace plot.
#' @param legend A list with multiple `gspace` legends.
#' @param spacer A grid::unit() value specifying space inserted between 
#' merged legends.
#' @param position Placement of legend relative to the main plot.
#' @param custom_theme An optional `ggplot2` theme object applied to the
#' legend panel. This allows customizing the appearance of the legend without
#' affecting the main plot.
#' 
#' @return
#' The `make_gspace_legend()` function returns a standalone legend, while the
#' `add_gspace_legend()` function returns a patchwork object containing a plot
#' combined with the generated legend in the specified position.
#' 
#' @details
#' The object returned by `add_gspace_legend()` behaves like a ggplot for saving,
#' combining with patchwork, arranging in grids, and rendering in standard
#' graphics devices. Internally, however, the original ggplot is converted into
#' a static 'grob' embedded inside a blank ggplot. As a result, the returned 
#' object cannot accept new layers, scales, or other ggplot modifications; it 
#' is no longer an editable ggplot, but a wrapped graphical representation 
#' of the composed figure.
#' 
#' @examples
#' library(ggplot2)
#' library(patchwork)
#' 
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' 
#' # Add a discrete standalone legend to a plot
#' leg1 <- make_gspace_legend(c(A="red", B="blue"), legend_size = 5)
#' p + leg1 + plot_layout(widths = c(1, 0.1))
#' 
#' # Add a continuous standalone legend to a plot
#' leg2 <- make_gspace_legend(c("blue", "white","red"), breaks = c(0, 1, 2))
#' p + leg2 + plot_layout(widths = c(1, 0.1))
#' 
#' # Add a legend list to a plot
#' legs <- list(leg1, leg2)
#' add_gspace_legend(plot = p, legs, position = "right")
#' 
#' @importFrom ggplot2 ggplot is_ggplot annotation_custom 
#' @importFrom ggplot2 coord_cartesian theme_void theme
#' @importFrom cowplot get_legend
#' @importFrom grid grobWidth convertWidth unit unit.pmax is.unit
#' @importFrom gtable gtable
#' @importFrom patchwork wrap_plots
#' @importFrom circlize colorRamp2
#' @rdname legend-accessors
#' @aliases make_gspace_legend
#' @aliases add_gspace_legend
#' @export
make_gspace_legend <- function(color_palette, breaks = NULL, 
  legend_title = "Title", legend_shape = 21, legend_size = 6,
  text_size = 10, orientation = c("vertical", "horizontal"),
  justification = c("left", "right"), 
  custom_theme = theme()) {

  .validate_gs_colors("allColors", "color_palette", color_palette)
  .validate_gs_args("singleString", "legend_title", legend_title)
  .validate_gs_args("singleNumber", "legend_size", legend_size)
  .validate_gs_args("integer_vec", "legend_shape", legend_shape)
  .validate_gs_args("singleNumber", "text_size", text_size)
  orientation <- match.arg(orientation)
  justification <- match.arg(justification)
  if(!ggplot2::is_theme(custom_theme)) 
    stop("'custom_theme' must be a 'ggplot2::theme' object.")
  
  if(is.null(breaks)){
    if(length(color_palette)<2)
      stop("'color_palette' must contain at least 2 colors.")
    if (is.null(names(color_palette)))
      names(color_palette) <- color_palette
    p <- .discrete_color_legend(color_palette, legend_shape, 
      legend_size, legend_title)
  } else {
    .validate_gs_args("numeric_vec", "breaks", breaks)
    if(length(breaks)<3) stop("'breaks' must contain at least 3 values.")
    if(length(color_palette)!= length(breaks)) 
      stop("'breaks' and 'color_palette' must have the same length.")
    p <- .continuous_color_legend(color_palette, breaks, legend_title)
  }
  hjust <- ifelse(justification=="left", 0, 1)
  text_size <- max(text_size, 1)
  position <- ifelse(orientation=="vertical", "right", "bottom")
  
  p <- p + ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = position, legend.box.just = justification,
      legend.title = ggplot2::element_text(size = text_size + 1, hjust = hjust),
      legend.text  = ggplot2::element_text(size = text_size),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      legend.spacing = ggplot2::unit(0.1, "mm"),
      legend.spacing.y = ggplot2::unit(0.1, "mm"),
      legend.spacing.x = ggplot2::unit(0.1, "mm"),
      legend.key.spacing = ggplot2::unit(0.1, "mm"), 
      legend.key.size = ggplot2::unit(legend_size + 1, "mm"),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    ) + custom_theme
  
  # Extract the legend grob
  leg <- cowplot::get_legend(p)
  class(leg) <- c(class(leg), "gspace_legend")
  return(leg)
  
}


#-------------------------------------------------------------------------------
#' @rdname legend-accessors
#' @aliases add_gspace_legend
#' @export
add_gspace_legend <- function(plot, legend, spacer = unit(2, "mm"), 
  position = c("right", "top", "bottom", "left")) {
  
  if(!ggplot2::is_ggplot(plot)) stop("'plot' must be a ggplot object.")
  if(!grid::is.unit(spacer)) stop("'plot' must be a ggplot object.")
  
  position <- match.arg(position)
  
  if(is.list(legend)){
    lapply(legend, function(leg){
      if( !inherits(leg, "gspace_legend") ){
        stop("'legend' must be a list with 'gspace_legend' objects.")
      }
    })
    
  } else {
    
    if( !inherits(legend, "gspace_legend") ){
      stop("'legend' must be a 'gspace_legend' object.")
    }
    
    legend <- list( legend )
    
  }
  
  # make a color legend
  len <- vapply(seq_along(legend), function(i){
    leg <- legend[[i]]
    w  <- grid::convertWidth(grid::grobWidth(leg), "mm", valueOnly = TRUE)
    h  <- grid::convertHeight(grid::grobHeight(leg), "mm", valueOnly = TRUE)
    c(w=w, h=h)
  }, numeric(2))
  len_sum <- apply(len, 1, sum)
  len_max <- apply(len, 1, max)
  
  # Convert to reasonable fractions
  ref_mm <- 100
  frac_h <- min(0.35, max(0.05, len_max["h"] / ref_mm))  # for top/bottom
  frac_w <- min(0.35, max(0.05, len_max["w"] / ref_mm))  # for left/right
  
  # Add to plot
  if (position == "top") {
    legs <- .merge_horizontal(legend, spacer)
    combined <- patchwork::wrap_plots(legs, plot, heights = c(frac_h, 1))
  } else if (position == "bottom"){
    legs <- .merge_horizontal(legend, spacer)
    combined <- patchwork::wrap_plots(plot, legs, heights = c(1, frac_h))
  } else if (position == "left"){
    legs <- .merge_vertical(legend, spacer)
    combined <- patchwork::wrap_plots(legs, plot, widths = c(frac_w, 1))
  } else if (position == "right"){
    legs <- .merge_vertical(legend, spacer)
    combined <- patchwork::wrap_plots(plot, legs, widths = c(1, frac_w))
  }
  combined <- patchwork::patchworkGrob(combined)
  combined <- ggplot() + annotation_custom(combined) + 
    coord_cartesian(clip = "off") + theme_void()
  
  return(combined)
  
}

#-------------------------------------------------------------------------------
.continuous_color_legend <- function(color_palette, breaks, legend_title){
  
  # Small legend data frame
  legend_df <- data.frame(
    x = seq_along(breaks),
    y = seq_along(breaks),
    z = breaks)
  
  cramp <- circlize::colorRamp2(breaks = breaks, colors = color_palette)
  bks <- .pretty_centered(breaks)
  pal <- cramp(bks)
  
  x <- y <- z <- NULL
  p <- ggplot2::ggplot(legend_df,
    ggplot2::aes(x = x, y = y, fill = z) ) +
    ggplot2::geom_point(
      shape = 21,
      size = 6,
      colour = "grey50") +
    ggplot2::scale_fill_gradientn(
      colours = color_palette,
      name = legend_title,
      values = scales::rescale(bks),
      breaks = bks,
      limits  = range(bks)
    )
  
  return(p)
  
}

#-------------------------------------------------------------------------------
.pretty_centered <- function(breaks){
  bks <- pretty(breaks, n=7)
  lo  <- min(bks)
  hi  <- max(bks)
  if(length(bks) %% 2 == 1){
    mid <- bks[(length(bks) + 1) / 2]
  } else {
    mid <- mean(bks[(length(bks)/2):(length(bks)/2 + 1)])
  }
  tol <- abs(hi - lo) * 0.1
  if (abs(mid) < tol) mid <- 0
  bks <- c(lo, mid, hi)
  return(bks)
}

#-------------------------------------------------------------------------------
.discrete_color_legend <- function(color_palette, legend_shape,
  legend_size, legend_title){
  if(length(legend_shape)==1){
    .fun <- .discrete_color_legend1
  } else {
    if(length(unique(legend_shape))==1){
      .fun <- .discrete_color_legend1
      legend_shape <- legend_shape[1]
    } else {
      .fun <- .discrete_color_legend2
    }
  }
  .fun(color_palette, legend_shape, legend_size, legend_title)
}
.discrete_color_legend1 <- function(color_palette, legend_shape,
  legend_size, legend_title){
  
  # Small legend data frame
  legend_df <- data.frame(
    x = 1,
    y = seq_along(color_palette),
    color = unname(color_palette),
    label = names(color_palette)
  )
  
  # Temporary legend-producing plot
  x <- y <- color <- NULL
  p <- ggplot2::ggplot(legend_df,
    ggplot2::aes(x = x, y = y, fill = color, 
      colour = color) ) +
    ggplot2::geom_point(
      size = legend_size, 
      shape = legend_shape) +
    ggplot2::scale_fill_identity(
      name = legend_title,
      labels = legend_df$label,
      breaks = legend_df$color,
      guide = "legend"
    ) +
    ggplot2::scale_colour_identity(
      name = legend_title,
      labels = legend_df$label,
      breaks = legend_df$color,
      guide = "legend"
    )
  return(p)
}

#-------------------------------------------------------------------------------
.discrete_color_legend2 <- function(color_palette, legend_shape,
  legend_size, legend_title){
  
  # Small legend data frame
  legend_df <- data.frame(
    x = 1,
    y = seq_along(color_palette),
    color = unname(color_palette),
    shape = legend_shape,
    label = names(color_palette)
  )
  
  # Temporary legend-producing plot
  x <- y <- color <- shape <- NULL
  p <- ggplot2::ggplot(legend_df,
    ggplot2::aes(x = x, y = y, fill = color, 
      colour = color, shape = shape) ) +
    ggplot2::geom_point(size = legend_size) +
    ggplot2::scale_fill_identity(
      name = legend_title,
      labels = legend_df$label,
      breaks = legend_df$color,
      guide = "legend"
    ) +
    ggplot2::scale_colour_identity(
      name = legend_title,
      labels = legend_df$label,
      breaks = legend_df$color,
      guide = "legend"
    ) +
    scale_shape_identity(name = legend_title, 
      labels = legend_df$label,
      breaks = legend_df$shape,
      guide = "legend")
  return(p)
}

#-------------------------------------------------------------------------------
.merge_vertical <- function(legs, spacer = grid::unit(2, "mm")) {
  if (length(legs) == 1) return(legs[[1]])
  all_widths <- do.call(grid::unit.pmax, lapply(legs, function(g) g$widths))
  spacer_g <- gtable::gtable(
    widths  = all_widths,
    heights = spacer
  )
  out <- vector("list", length(legs) * 2 - 1)
  out[seq(1, length(out), by = 2)] <- legs
  idx <- sapply(out, is.null)
  out[idx] <- list(spacer_g)
  out <- Reduce(function(x, y) rbind(x, y, size = "max"), out)
  return(out)
}

.merge_horizontal <- function(legs, spacer = grid::unit(2, "mm")) {
  if (length(legs) == 1) return(legs[[1]])
  all_heights <- do.call(grid::unit.pmax, lapply(legs, function(g) g$heights))
  spacer_g <- gtable::gtable(
    widths  = spacer,
    heights = all_heights
  )
  out <- vector("list", length(legs) * 2 - 1)
  out[seq(1, length(out), by = 2)] <- legs
  idx <- sapply(out, is.null)
  out[idx] <- list(spacer_g)
  out <- Reduce(function(x, y) cbind(x, y, size = "max"), out)
  return(out)
}




