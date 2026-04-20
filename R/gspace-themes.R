
#-------------------------------------------------------------------------------
#' RGraphSpace ggplot2 themes
#'
#' A set of \pkg{ggplot2} themes used by \pkg{RGraphSpace} plots.
#' 
#' @param txt_size Numeric value to scale plot- and axis-related text elements.
#' @param leg_size Numeric value to scale legend-related elements.
#' @param bg_color A color name or hex code specifying the panel background.
#' @param key_fill Logical; if TRUE, treats the fill legend as discrete 
#' to adjust key size.
#' @param key_colour Logical; if TRUE, treats the colour legend as discrete 
#' to adjust key size.
#' @param ... Additional arguments passed to \code{theme_gspace_th*} and
#' \link[ggplot2]{ggtheme}.
#' @details
#' \code{theme_gspace_th0()} is a minimal wrapper around 
#' \link[ggplot2]{theme_gray} that simplifies axis and legend scaling.
#' The \code{txt_size} and \code{leg_size} arguments aggregate related 
#' \link[ggplot2]{theme} parameters for quick thematic overrides.
#' 
#' \code{theme_gspace_th1()} builds on \code{theme_gspace_th0()} and 
#' modifies grid lines, axis appearance, and panel borders.
#'
#' \code{theme_gspace_th2()} is similar to \code{theme_gspace_th1()} with
#' simplified grid elements and a customizable panel background.
#'
#' \code{theme_gspace_th3()} is similar to \code{theme_gspace_th2()} but with
#' slightly adjusted margins, tick appearance, and legend formatting.
#'
#' The \code{theme_gspace_coords()} is a helper function that also adds axes
#' scales for normalized coordinates. It configures axis breaks, limits, 
#' and expansion for graph layouts. Plot coordinates are ideally normalized 
#' to the interval \code{[0, 1]}.
#' 
#' @return
#' \code{theme_gspace_th*()} return a \code{ggplot2} theme object.
#' 
#' \code{theme_gspace_coords()} returns a list containing scale and theme
#' components that can be added to a \pkg{ggplot2} plot.
#'
#' @seealso \link[ggplot2]{ggtheme}, \link[ggplot2]{theme}
#' 
#' @examples
#' library(ggplot2)
#' 
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_gspace_th0()
#'   
#' ggplot(mtcars, 
#'   aes(scales::rescale(wt), 
#'       scales::rescale(mpg))) +
#'   geom_point() +
#'   theme_gspace_coords("th2", is_norm = TRUE)
#'   
#' @importFrom ggplot2 "%+replace%"
#' @name theme_gspace
#' @export
theme_gspace_th0 <- function(txt_size = 1, leg_size = 1, 
  bg_color = "grey95", key_fill = FALSE,
  key_colour = FALSE, ...) {
  
  et0 <- ggplot2::element_text(size = 14 * txt_size)
  et1 <- ggplot2::element_text(size = 12 * txt_size)
  et2 <- ggplot2::element_text(size = 11 * txt_size)
  et3 <- ggplot2::element_text(size = 9 * leg_size)
  et4 <- ggplot2::element_text(size = 8 * leg_size)
  et1s <- ggplot2::element_text(size = 12 * txt_size, hjust = 0)
  
  l_th <- list()
  l_th[[1]] <- ggplot2::theme_gray(...) %+replace%
  ggplot2::theme(
    plot.title = et0, plot.subtitle = et1s,
    axis.title = et1, axis.text = et2,
    
    legend.title = et3, legend.text = et4,
    legend.key.spacing = unit(leg_size, "mm"),
    legend.spacing = unit(leg_size, "mm"),
    legend.key.size = unit(3 * leg_size, "mm"),
    legend.margin = margin(leg_size, leg_size, leg_size, leg_size),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    
    panel.background = element_rect(fill = bg_color, colour = NA),
    aspect.ratio = 1, complete = TRUE)
  
  l_args <- .guide_args(leg_size, key_fill, key_colour)
  if (length(l_args) > 0) {
    l_th[[2]] <- do.call(guides, l_args)
  }
  
  return(l_th)
  
}

#-------------------------------------------------------------------------------
#' @rdname theme_gspace
#' @export
theme_gspace_th1 <- function(txt_size = 1, leg_size = 1, 
  bg_color = "grey95", key_fill = FALSE,
  key_colour = FALSE, ...) {
  
  et0 <- ggplot2::element_text(size = 14 * txt_size)
  et1 <- ggplot2::element_text(size = 12 * txt_size)
  et2 <- ggplot2::element_text(size = 11 * txt_size)
  et3 <- ggplot2::element_text(size = 9 * leg_size)
  et4 <- ggplot2::element_text(size = 8 * leg_size)
  et1s <- ggplot2::element_text(size = 12 * txt_size, hjust = 0)
  
  l_th <- list()
  l_th[[1]] <- ggplot2::theme_gray(...) %+replace%
    ggplot2::theme(
      plot.title = et0, plot.subtitle = et1s,
      axis.title = et1, axis.text = et2,
      
      legend.title = et3, legend.text = et4,
      legend.key.spacing = unit(leg_size, "mm"),
      legend.spacing = unit(leg_size, "mm"),
      legend.key.size = unit(3 * leg_size, "mm"),
      legend.margin = margin(leg_size, leg_size, leg_size, leg_size),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      
      panel.background = element_rect(fill = bg_color, colour = NA),
      panel.grid.minor = element_line(linewidth = 0.7),
      panel.grid.major = element_line(linewidth = 0.7),
      panel.border = element_rect(linewidth = 1.2),
      
      axis.ticks = element_line(linewidth = 0.7),
      axis.line = element_blank(),
      plot.margin = margin(1, 1, 1, 1), 
      aspect.ratio = 1, complete = TRUE)
  
  l_args <- .guide_args(leg_size, key_fill, key_colour)
  if (length(l_args) > 0) {
    l_th[[2]] <- do.call(guides, l_args)
  }
  return(l_th)
}

#-------------------------------------------------------------------------------
#' @rdname theme_gspace
#' @export
theme_gspace_th2 <- function(txt_size = 1, leg_size = 1, 
  bg_color = "grey95", key_fill = FALSE, 
  key_colour = FALSE, ...) {
  
  et0 <- ggplot2::element_text(size = 14 * txt_size)
  et1 <- ggplot2::element_text(size = 12 * txt_size)
  et2 <- ggplot2::element_text(size = 11 * txt_size)
  et3 <- ggplot2::element_text(size = 9 * leg_size)
  et4 <- ggplot2::element_text(size = 8 * leg_size)
  et1s <- ggplot2::element_text(size = 12 * txt_size, hjust = 0)
  
  l_th <- list()
  l_th[[1]] <- ggplot2::theme_gray(...) %+replace%
    ggplot2::theme(
      plot.title = et0, plot.subtitle = et1s,
      axis.title = et1, axis.text = et2,
      
      legend.title = et3, legend.text = et4,
      legend.key.spacing = unit(leg_size, "mm"),
      legend.spacing = unit(leg_size, "mm"),
      legend.key.size = unit(3 * leg_size, "mm"),
      legend.margin = margin(leg_size, leg_size, leg_size, leg_size),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      
      panel.background = element_rect(fill = bg_color, colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.75),
      axis.ticks = element_line(linewidth = 0.7),
      
      axis.line = element_blank(), panel.border = element_blank(),
      plot.margin = margin(5, 10, 0, 10), 
      aspect.ratio = 1, complete = TRUE)
  
  l_args <- .guide_args(leg_size, key_fill, key_colour)
  if (length(l_args) > 0) {
    l_th[[2]] <- do.call(guides, l_args)
  }
  return(l_th)
}

#-------------------------------------------------------------------------------
#' @rdname theme_gspace
#' @export
theme_gspace_th3 <- function(txt_size = 1, leg_size = 1, 
  bg_color = "grey95", key_fill = FALSE, 
  key_colour = FALSE, ...) {
  
  et0 <- ggplot2::element_text(size = 14 * txt_size)
  et1 <- ggplot2::element_text(size = 12 * txt_size)
  et2 <- ggplot2::element_text(size = 11 * txt_size)
  et3 <- ggplot2::element_text(size = 9 * leg_size)
  et4 <- ggplot2::element_text(size = 8 * leg_size)
  et1s <- ggplot2::element_text(size = 12 * txt_size, hjust = 0)
  
  l_th <- list()
  l_th[[1]] <- ggplot2::theme_gray(...) %+replace%
    ggplot2::theme(
      plot.title = et0, plot.subtitle = et1s,
      axis.title = et1, axis.text = et2,
      
      legend.title = element_text(size = 9 * leg_size, vjust = 1), 
      legend.text = element_text(size = 8 * leg_size, vjust = 1),
      legend.key.spacing = unit(leg_size, "mm"), 
      legend.spacing = unit(leg_size, "mm"),
      legend.key.size = unit(3 * leg_size, "mm"),
      legend.margin = margin(leg_size, leg_size, leg_size, leg_size),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.box.margin = margin(0, 0, 0, 0),
      
      panel.background = element_rect(fill = bg_color, colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.75),
      
      axis.ticks = element_line(linewidth = 0.5),
      axis.line = element_blank(), panel.border = element_blank(),
      plot.margin = margin(5, 5, 5, 5),
      aspect.ratio = 1, complete = TRUE)
  
  l_args <- .guide_args(leg_size, key_fill, key_colour)
  if (length(l_args) > 0) {
    l_th[[2]] <- do.call(guides, l_args)
  }
  return(l_th)
}

#' @param theme Character string specifying the GraphSpace theme variant. 
#' Options: `th0`, `th1`, `th2`, and `th3`.
#' @param is_norm Logical; if TRUE, assumes plot coordinates are 
#' already normalized in \code{[0, 1]}.
#' @param xlab The title for the 'x' axis.
#' @param ylab The title for the 'y' axis.
#' @param expand A range expansion factor applied to both the lower and 
#' upper limits of the 'x' and 'y' scales.
#' @rdname theme_gspace
#' @export
theme_gspace_coords <- function(theme = "th0", is_norm = FALSE,
  xlab = "Graph coordinates 1", ylab = "Graph coordinates 2", 
  expand = NULL, ...) {
  
  theme <- match.arg(theme, choices = c("th0", "th1", "th2", "th3"))
  bks <- .set_theme_bks(theme)
  bks$expand <- expand %||% bks$expand
  th <- list()
  if(is_norm){
    th[[length(th)+1]] <- scale_x_continuous(breaks = bks$axis.ticks,
      labels = format(bks$axis.ticks), position = bks$x.position,
      limits = bks$xylim, expand = ggplot2::expansion(mult = bks$expand))
    th[[length(th)+1]] <- scale_y_continuous(breaks = bks$axis.ticks,
      labels = format(bks$axis.ticks), limits = bks$xylim,
      expand = ggplot2::expansion(mult = bks$expand))
  } else {
    th[[length(th)+1]] <- scale_x_continuous(position = bks$x.position)
  }
  ftheme <- .get_gspace_theme(theme)
  th[[length(th)+1]] <- ftheme(...)
  th[[length(th)+1]] <- labs(x = xlab, y = ylab)
  attr(th, "gspace_pars") <- bks
  return(th)
}
.get_gspace_theme <- function(theme) {
  if (theme == "th3") {
    ftheme <- theme_gspace_th3
  } else if (theme == "th2") {
    ftheme <- theme_gspace_th2
  } else if (theme == "th1") {
    ftheme <- theme_gspace_th1 
  } else {
    ftheme <- theme_gspace_th0
  }
  return(ftheme)
}
.set_theme_bks <- function(theme){
  bks <- list()
  if (theme %in% c("th3")) {
    bks$axis.ticks <- c(0.25, 0.5, 0.75)
    bks$xylim <- c(0, 1)
    bks$expand <- 0.01
    bks$x.position <- "top"
    bks$justify <- "centre"
    bks$leg.position <- "bottom"
  } else if (theme %in% c("th2")) {
    bks$axis.ticks <- seq(0.1, 0.9, 0.2)
    bks$xylim <- c(0, 1)
    bks$expand <- 0.02
    bks$x.position <- "bottom"
    bks$justify <- "right"
    bks$leg.position <- "right"
  } else {
    bks$axis.ticks <- seq(0, 1, 0.2)
    bks$xylim <- c(0, 1)
    bks$expand <- 0.05
    bks$x.position <- "bottom"
    bks$justify <- "right"
    bks$leg.position <- "right"
  }
  return(bks)
}

#-------------------------------------------------------------------------------
#' @details
#' \code{theme_gspace_legend()} is helper function that adjusts legend text,
#' title, and key sizes by a single scaling factor.
#' 
#' @return
#' \code{theme_gspace_legend()} returns a list of theme and guide components.
#' 
#' @examples
#' # Small scale legends
#' ggplot(mtcars, aes(wt, mpg, fill = factor(cyl))) + 
#'   geom_point(shape = 21) + 
#'   theme_gspace_legend(0.8)
#'
#' @importFrom ggplot2 theme element_text unit guides guide_legend
#' @rdname theme_gspace
#' @export
theme_gspace_legend <- function(leg_size = 1, 
  key_fill = FALSE, key_colour = FALSE, ...) {
  
  et1 <- ggplot2::element_text(size = 9 * leg_size)
  et2 <- ggplot2::element_text(size = 8 * leg_size)
  
  l_th <- list(
    theme(
      legend.title = et1, legend.text = et2,
      legend.key.spacing = unit(2*leg_size, "mm"),
      legend.spacing = unit(leg_size, "mm"),
      legend.key.size = unit(3 * leg_size, "mm"),
      legend.margin = margin(2*leg_size, 2*leg_size, 2*leg_size, 2*leg_size), 
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.box.margin = margin(0, 0, 0, 0), 
    )
  )
  
  l_args <- .guide_args(leg_size, key_fill, key_colour)
  if (length(l_args) > 0) {
    l_th[[length(l_th)+1]] <- do.call(guides, l_args)
  }
  return(l_th)
  
}

#-------------------------------------------------------------------------------
.guide_args <- function(leg_size = 1, key_fill = FALSE, key_colour = FALSE) {
  l_args <- list()
  l_args$shape <- guide_legend(override.aes = list(size = 3 * leg_size))
  l_args$linetype <- guide_legend(
    theme = theme(legend.key.width = unit(10 * leg_size, "mm")),
    override.aes = list(size = 3 * leg_size))
  if (key_fill) {
    l_args$fill <- guide_legend(override.aes = list(size = 3 * leg_size))
  }
  if (key_colour) {
    l_args$colour <- guide_legend(override.aes = list(size = 3 * leg_size))
  }
  l_args
}
