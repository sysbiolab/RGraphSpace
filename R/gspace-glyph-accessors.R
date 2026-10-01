#-------------------------------------------------------------------------------
#' Glyph mode of arrowType codes
#'
#' For each \code{arrowType} code, which edge ends carry a glyph, as a
#' numeric mode.
#'
#' @param arrowType A vector of \code{arrowType} codes (integer codes or
#' token codes; see \code{\link{GraphSpace}}).
#' @return An integer vector: \code{0} no glyph, \code{1} a glyph at the
#' end only, \code{2} at the start only, \code{3} at both ends. Note that
#' \code{1} and \code{2} are the reverse of igraph's \code{arrow.mode},
#' where \code{1} is a backward arrow.
#' @examples
#' glyph_mode(c("-->", "<--", "<->", "---", "04|->03"))
#' @export
glyph_mode <- function(arrowType) .get_emode(arrowType)

#-------------------------------------------------------------------------------
#' List available edge glyphs
#'
#' The edge glyphs available for use in \code{arrowType} codes. This is the
#' RGraphSpace glyph vocabulary, discovered from the built-in \code{Glyph*}
#' objects at package load.
#'
#' @return A data frame of class \code{gs_glyph_list}, one row per token.
#' @seealso \code{\link{glyph_legend}}
#' @examples
#' 
#' # List the glyphs as a data frame
#' glyphs <- glyph_list()
#' 
#' # Plot the glyphs for visual inspection
#' plot(glyphs)
#' 
#' # Plot the glyphs in one column per group
#' plot(glyphs, by_group = TRUE)
#' 
#' @export
glyph_list <- function() {
  
  # get glyph prototypes
  v <- .glyph_vocab()
  
  # extract descriptors
  df <- data.frame(
    token = names(v),
    name = vapply(v, function(g) g$name, character(1)),
    group  = vapply(v, function(g) g$group,  character(1)),
    draw  = vapply(v, function(g) g$draw,  character(1)),
    row.names = NULL, stringsAsFactors = FALSE)
  
  # sort by group (basic, vee-like, tee-like, then any other), then by number,
  # which keeps each filled (odd) and open (even) pair together; order
  # columns and assign a class
  group_order <- c("basic", "vee-like", "tee-like")
  group_order <- c(group_order, setdiff(unique(df$group), group_order))
  df <- df[order(match(df$group, group_order), df$token, method = "radix"),
    c("token", "name", "group", "draw"), drop = FALSE]
  rownames(df) <- NULL
  class(df) <- c("gs_glyph_list", class(df))
  
  df
}

#' @rdname glyph_list
#' @param x A \code{gs_glyph_list} object, as returned by \code{glyph_list()}.
#' @param ncol Number of columns of keys (ignored when \code{by_group = TRUE}).
#' @param legend_title Legend title (ignored when \code{by_group = TRUE}).
#' @param by_group Logical; if \code{TRUE}, draw one column per glyph group,
#' each titled with the group's name.
#' @param glyph_size,key_width,text_size Sizes passed to
#' \code{\link{glyph_legend}}; reduced if needed to fit the plotting area.
#' @param ... Further arguments passed to \code{\link{glyph_legend}}, such as
#' \code{colour}.
#' @export
plot.gs_glyph_list <- function(x, ncol = 3, legend_title = "Edge glyphs",
  by_group = FALSE, glyph_size = 3, key_width = 12, text_size = 10, ...) {
  grid::grid.newpage()
  # Build the legend, shrinking it until it fits the plotting area
  s <- 1
  repeat {
    leg <- .glyph_list_legend(x, ncol, legend_title, by_group,
      s * glyph_size, s * key_width, s * text_size, ...)
    fit <- min(
      grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE) /
        grid::convertWidth(sum(leg$widths), "mm", valueOnly = TRUE),
      grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE) /
        grid::convertHeight(sum(leg$heights), "mm", valueOnly = TRUE))
    if (fit >= 1 || s < 0.2) break
    s <- s * fit * 0.95
  }
  grid::grid.draw(leg)
  invisible(leg)
}

# Build the legend drawn by plot.gs_glyph_list()
.glyph_list_legend <- function(x, ncol, legend_title, by_group,
  glyph_size, key_width, text_size, ...) {
  codes <- stats::setNames(x$token, paste(x$token, x$name))
  if (!by_group || is.null(x$group)) {
    return(glyph_legend(codes, ncol = ncol, legend_title = legend_title,
      glyph_size = glyph_size, key_width = key_width, text_size = text_size,
      ...))
  }
  groups <- unique(x$group)
  # Column titles: the group names, capitalised
  titles <- paste0(toupper(substring(groups, 1L, 1L)), substring(groups, 2L))
  legs <- lapply(seq_along(groups), function(i) {
    l <- glyph_legend(codes[x$group == groups[i]], legend_title = titles[i],
      glyph_size = glyph_size, key_width = key_width, text_size = text_size,
      ...)
    # top-align the group's table within its column
    tab <- l$grobs[[1]]
    l$grobs[[1]]$vp <- grid::viewport(y = 1, just = "top",
      height = sum(tab$heights))
    # spacer between group columns
    if (i < length(groups)) {
      l <- gtable::gtable_add_cols(l, grid::unit(6, "mm"))
    }
    l
  })
  do.call(cbind, c(legs, size = "max"))
}
