
#' RGraphSpace ggplot2 themes
#'
#' A set of simple \pkg{ggplot2} themes used by \pkg{RGraphSpace} plots.
#' These themes control text scaling, panel appearance, and grid elements
#' for visualizing graph-based layouts.
#'
#' All themes scale text elements relative to \code{base_size} and allow
#' customization of the panel background color through \code{bg_color}.
#' 
#' The \code{theme_gspace_axes()} is a helper function that also adds axes
#' scales for normalized coordinates. 
#' 
#' @param base_size Numeric value used to scale text elements in the theme.
#' @param bg_color Character string specifying the background color used in
#'   panel elements such as the panel background or grid lines.
#' @param theme Character string specifying the GraphSpace theme variant.
#' @param xlab The title for the 'x' axis of a 2D-image space.
#' @param ylab The title for the 'y' axis of a 2D-image space.
#' 
#' @details
#' \code{theme_gspace_th0()} provides a minimal theme with a customizable
#' panel background and hidden legends.
#'
#' \code{theme_gspace_th1()} builds on \code{ggplot2::theme_bw()} and modifies
#' grid lines, axis appearance, and panel borders.
#'
#' \code{theme_gspace_th2()} builds on \code{ggplot2::theme_gray()} with
#' simplified grid elements and a customizable panel background.
#'
#' \code{theme_gspace_th3()} is similar to \code{theme_gspace_th2()} but with
#' slightly adjusted margins, tick appearance, and legend formatting.
#'
#' \code{theme_gspace_axes()} adds axis scales and styling consistent with 
#' the selected GraphSpace theme. This helper configures axis breaks, limits, 
#' and expansion for graph layouts and applies the corresponding theme. 
#' Coordinates must be normalized to the interval \code{[0, 1]}.
#' 
#' @return
#' \code{theme_gspace_th*()} return a \code{ggplot2} theme object.
#' \code{theme_gspace_axes()} returns a list containing scale and theme
#' components that can be added to a \pkg{ggplot2} plot with coordinates 
#' normalized to \code{[0, 1]}.
#'
#' @examples
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_gspace_th0()
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_gspace_th1()
#'   
#' ggplot(mtcars, 
#'   aes(scales::rescale(wt), 
#'       scales::rescale(mpg))) +
#'   geom_point() +
#'   theme_gspace_axes("th1")
#'   
#' @name theme_gspace
#' @export
theme_gspace_th0 <- function(base_size = 1, bg_color = "grey95") {
  et1 <- ggplot2::element_text(size = 14 * base_size)
  et2 <- ggplot2::element_text(size = 12 * base_size)
  ggplot2::theme(axis.title = et1, axis.text = et2,
    legend.title = et2, legend.text = et2, 
    panel.background = element_rect(fill = bg_color),
    aspect.ratio = 1, legend.position = "none")
}

#' @rdname theme_gspace
#' @export
theme_gspace_th1 <- function(base_size = 1, bg_color = "grey95") {
  et1 <- ggplot2::element_text(size = 14 * base_size)
  et2 <- ggplot2::element_text(size = 12 * base_size)
  ggplot2::theme_bw() +
    ggplot2::theme(axis.title = et1,
      axis.text = et2, legend.title = et2,
      legend.text = et2, legend.margin = margin(0, 0, 0, 0), 
      plot.margin = margin(1, 1, 1, 1), 
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      panel.grid.minor = element_line(linewidth = 0.7, 
        colour = bg_color),
      panel.grid.major = element_line(linewidth = 0.7,
        colour = bg_color),
      axis.ticks = element_line(linewidth = 0.7),
      axis.line = element_blank(),
      panel.border = element_rect(linewidth = 1.2),
      aspect.ratio = 1, legend.position = "none")
}

#' @rdname theme_gspace
#' @export
theme_gspace_th2 <- function(base_size = 1, bg_color = "grey95") {
  et1 <- ggplot2::element_text(size = 14 * base_size)
  et2 <- ggplot2::element_text(size = 12 * base_size)
  ggplot2::theme_gray() +
    ggplot2::theme(axis.title = et1, axis.text = et2, legend.title = et2,
    legend.text = et2, legend.margin = margin(0, 0, 0, 0), 
    plot.margin = margin(5, 10, 0, 10), 
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.75),
    axis.ticks = element_line(linewidth = 0.7),
    axis.line = element_blank(), panel.border = element_blank(),
    panel.background = element_rect(fill = bg_color),
    aspect.ratio = 1, legend.position = "none")
  
    #ggplot2::theme(panel.grid.major = element_blank())
  
}

#' @rdname theme_gspace
#' @export
theme_gspace_th3 <- function(base_size = 1, bg_color = "grey95") {
  et1 <- ggplot2::element_text(size = 14 * base_size)
  et2 <- ggplot2::element_text(size = 12 * base_size)
  ggplot2::theme_gray() + 
    ggplot2::theme(axis.title = et1, axis.text = et2, 
      legend.title = element_text(size = 12 * base_size, vjust = 1), 
      legend.text = et2,
      legend.margin = margin(0, 0, 0, 0),
      legend.key.height = grid::unit(5, "mm"),
      plot.margin = margin(5, 5, 5, 5),
      legend.box.margin = margin(0, 0, 0, 0), 
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.75),
      axis.ticks = element_line(linewidth = 0.5),
      axis.line = element_blank(), panel.border = element_blank(),
      panel.background = element_rect(fill = bg_color),
      aspect.ratio = 1, legend.position = "none")
  
  #ggplot2::theme(panel.grid.major = element_blank())
  
}

#' @rdname theme_gspace
#' @export
theme_gspace_axes <- function(theme = c("th0", "th1", "th2", "th3"),
  base_size = 1, bg_color = "grey95", xlab = "Graph coordinates 1", 
  ylab = "Graph coordinates 2") {
  theme <- match.arg(theme)
  bks <- .set_theme_bks(theme)
  th <- list(
    scale_x_continuous(breaks = bks$axis.ticks,
      labels = format(bks$axis.ticks), position = bks$x.position,
      limits = bks$xylim, expand = ggplot2::expansion(mult = 0)),
    scale_y_continuous(breaks = bks$axis.ticks,
      labels = format(bks$axis.ticks), limits = bks$xylim,
      expand = ggplot2::expansion(mult = 0)),
    .theme_gspace_axes(theme, base_size, bg_color),
    labs(x = xlab, y = ylab)
  )
  attr(th, "gspace_pars") <- bks
  return(th)
}
.theme_gspace_axes <- function(theme, base_size, bg_color) {
  if (theme == "th3") {
    th <- theme_gspace_th3(base_size, bg_color)
  } else if (theme == "th2") {
    th <- theme_gspace_th2(base_size, bg_color)
  } else if (theme == "th1") {
    th <- theme_gspace_th1(base_size, bg_color) 
  } else {
    th <- theme_gspace_th0(base_size, bg_color)
  }
  return(th)
}
.set_theme_bks <- function(theme){
  bks <- list()
  if (theme %in% c("th3")) {
    bks$axis.ticks <- c(0.25, 0.5, 0.75)
    bks$xylim <- c(-0.01, 1.01)
    bks$x.position <- "top"
    bks$justify <- "centre"
    bks$leg.position <- "bottom"
  } else if (theme %in% c("th2")) {
    bks$axis.ticks <- seq(0.1, 0.9, 0.2)
    bks$xylim <- c(-0.01, 1.01)
    bks$x.position <- "bottom"
    bks$justify <- "right"
    bks$leg.position <- "right"
  } else {
    bks$axis.ticks <- seq(0, 1, 0.2)
    bks$xylim <- c(-0.05, 1.05)
    bks$x.position <- "bottom"
    bks$justify <- "right"
    bks$leg.position <- "right"
  }
  return(bks)
}

