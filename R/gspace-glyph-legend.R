#-------------------------------------------------------------------------------
#' @title Create a standalone legend for edge glyphs
#'
#' @description
#' Builds a standalone legend explaining the glyphs drawn at edge ends. Glyphs
#' are not mapped to 'ggplot2' aesthetics, so they never appear in a 'ggplot2'
#' legend; this function draws each key with the same renderer used for edges
#' and returns it as a 'grob' object that can be added to a plot.
#'
#' @param arrowType A named vector of arrowType codes, written as for edges;
#' names become legend labels. Codes may be integer codes (e.g. \code{1},
#' \code{-1}), basic token codes (e.g. \code{"-->"}, \code{"<->"}), or
#' extended token codes (e.g. \code{"01<->01"}); a bare token is read as the
#' end glyph (e.g. \code{">01"}). See \code{\link{glyph_list}} for the
#' available tokens.
#' @param legend_title The legend title, or \code{NULL} for no title.
#' @param glyph_size Glyph size, in mm.
#' @param key_width Width of each key (the edge sample), in mm. It should be
#' large enough to hold the glyphs at both ends, e.g. at least
#' \code{4 * glyph_size}.
#' @param text_size Text size, in points.
#' @param colour Edge and glyph colour: one value, or one per key.
#' @param linewidth Edge and glyph line width, in mm: one value, or one per
#' key.
#' @param orientation Legend arrangement (\code{"vertical"} or
#' \code{"horizontal"}): the order in which keys fill the columns.
#' Vertical fills each column top to bottom; horizontal fills each row
#' left to right.
#' @param ncol Number of columns of keys. Defaults to 1 for a vertical
#' legend and to one column per key (a single row) for a horizontal one.
#'
#' @return A 'gtable' object of class 'gspace_legend', which can be drawn with
#' \code{plot()} or \code{grid::grid.draw()}, or added to a ggplot with
#' 'patchwork'.
#'
#' @examples
#' library(ggplot2)
#' 
#' # Show the whole glyph collection, arranged in three columns
#' glyphs <- glyph_list()
#' leg1 <- glyph_legend(glyphs$token, ncol = 3, 
#'         legend_title = "Edge glyph collection")
#' plot(leg1)
#' 
#' # Build a legend from named arrowType codes; names become labels
#' tokens <- c(Activation = "-->", Inhibition = "--|", Complex = "03|-|05")
#' leg2 <- glyph_legend(tokens, legend_title = "Interaction")
#' plot(leg2)
#' 
#' # Add a glyph legend to a plot (requires patchwork)
#' if (requireNamespace("patchwork", quietly = TRUE)) {
#'   p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'   p + leg2 + patchwork::plot_layout(widths = c(1, 0.5))
#' }
#' 
#' @importFrom grid unit unit.c unit.pmax textGrob segmentsGrob gTree gList
#' @importFrom grid viewport nullGrob grobWidth gpar
#' @importFrom gtable gtable gtable_add_grob
#' @aliases glyph_legend
#' @export
glyph_legend <- function(arrowType, legend_title = NULL,
  glyph_size = 3, key_width = 12, text_size = 10,
  colour = "grey20", linewidth = 0.5,
  orientation = c("vertical", "horizontal"), ncol = NULL) {
  
  #--- Validate arguments
  if (!(is.character(arrowType) || is.numeric(arrowType)) ||
      length(arrowType) == 0L || anyNA(arrowType)) {
    rlang::abort("'arrowType' must be a non-empty character or numeric vector.")
  }
  if (is.numeric(arrowType) &&
      !all(arrowType %in% .int_arrowtypes(is.dir = FALSE))) {
    rlang::abort(c("Unknown integer code(s) in 'arrowType'.",
      i = "See the 'Arrowhead types' section of ?GraphSpace."))
  }
  if (!is.null(legend_title)) {
    .validate_gs_args("singleString", "legend_title", legend_title)
  }
  .validate_gs_args("singlePositiveNumber", "glyph_size", glyph_size)
  .validate_gs_args("singleNumber", "key_width", key_width)
  .validate_gs_args("singleNumber", "text_size", text_size)
  .validate_gs_colors("allColors", "colour", colour)
  orientation <- rlang::arg_match(orientation)
  n <- length(arrowType)
  if (is.null(ncol)) ncol <- if (orientation == "vertical") 1L else n
  if (!is.numeric(ncol) || length(ncol) != 1L || is.na(ncol) || ncol < 1) {
    rlang::abort("'ncol' must be a single positive number.")
  }
  ncol <- min(as.integer(ncol), n)
  if (!length(colour) %in% c(1L, n)) {
    rlang::abort("'colour' must have length 1 or one value per key.")
  }
  if (!is.numeric(linewidth) || anyNA(linewidth) ||
      !length(linewidth) %in% c(1L, n)) {
    rlang::abort("'linewidth' must be numeric, with length 1 or one per key.")
  }
  
  #--- Parse codes into start/end tokens, as for edges
  tk <- .arrowtype_to_tokens(arrowType)
  ok <- .valid_tokens(tk)
  if (!all(ok)) {
    rlang::abort(c(
      sprintf("Unknown glyph token(s) in 'arrowType': %s.",
        paste(sprintf("'%s'", unique(arrowType[!ok])), collapse = ", ")),
      .explain_tokens(setdiff(tk[!ok, ], names(.glyph_vocab()))),
      i = "See glyph_list() for the available tokens."))
  }
  
  #--- Labels: names of 'arrowType', falling back to the codes themselves
  labels <- names(arrowType)
  codes <- as.character(arrowType)
  if (is.null(labels)) labels <- codes
  labels[is.na(labels) | !nzchar(labels)] <-
    codes[is.na(labels) | !nzchar(labels)]
  
  #--- One key grob per entry
  colour <- rep_len(colour, n)
  linewidth <- rep_len(linewidth, n)
  vocab <- .glyph_vocab()
  keys <- lapply(seq_len(n), function(i) {
    .glyph_legend_key(vocab[[tk[i, "start"]]], vocab[[tk[i, "end"]]],
      glyph_size, colour[i], linewidth[i])
  })
  
  #--- Assemble
  .glyph_legend_layout(keys, labels, legend_title, glyph_size, key_width,
    text_size, orientation, ncol)
}

#-------------------------------------------------------------------------------
# Lay out keys, labels and an optional title in a gtable. Entries form a grid
# of `ncol` columns, each column being key | gap | label, with a spacer
# between columns; a vertical legend fills the grid column by column, a
# horizontal one row by row. The title spans all columns; a filler column
# widens the table if the title is wider than the entries.
.glyph_legend_layout <- function(keys, labels, legend_title, glyph_size,
  key_width, text_size, orientation, ncol) {
  
  n <- length(keys)
  nrow <- ceiling(n / ncol)
  if (orientation == "vertical") ncol <- ceiling(n / nrow)  # drop empty columns
  gap <- grid::unit(1.5, "mm")    # key -> label
  spacer <- grid::unit(4, "mm")   # between columns of entries
  row_h <- grid::unit.pmax(grid::unit(2 * glyph_size, "mm"),
    grid::unit(1.4 * text_size, "points"))
  key_w <- grid::unit(key_width, "mm")
  
  labs <- lapply(labels, function(lb) {
    grid::textGrob(lb, x = 0, hjust = 0,
      gp = grid::gpar(fontsize = text_size))
  })
  
  # Grid cell of each entry
  i <- seq_len(n) - 1L
  if (orientation == "vertical") {
    row <- i %% nrow + 1L;  col <- i %/% nrow + 1L
  } else {
    row <- i %/% ncol + 1L; col <- i %% ncol + 1L
  }
  
  # Widths: per column of entries, key | gap | widest label in that column
  widths <- do.call(grid::unit.c, lapply(seq_len(ncol), function(j) {
    lab_w <- do.call(grid::unit.pmax, lapply(labs[col == j], grid::grobWidth))
    w <- grid::unit.c(key_w, gap, lab_w)
    if (j < ncol) grid::unit.c(w, spacer) else w
  }))
  heights <- rep(row_h, nrow)
  key_l <- (col - 1L) * 4L + 1L
  lab_l <- (col - 1L) * 4L + 3L
  
  title <- NULL
  if (!is.null(legend_title)) {
    title <- grid::textGrob(legend_title, x = 0, hjust = 0,
      gp = grid::gpar(fontsize = text_size + 1))
    filler <- grid::unit.pmax(grid::unit(0, "mm"),
      grid::grobWidth(title) - sum(widths))
    widths <- grid::unit.c(widths, filler)
    heights <- grid::unit.c(grid::unit(1.6 * (text_size + 1), "points"),
      heights)
    row <- row + 1L
  }
  
  gt <- gtable::gtable(widths = widths, heights = heights)
  if (!is.null(title)) {
    gt <- gtable::gtable_add_grob(gt, title, t = 1, l = 1,
      r = length(widths), clip = "off", name = "title")
  }
  for (k in seq_len(n)) {
    gt <- gtable::gtable_add_grob(gt, keys[[k]], t = row[k], l = key_l[k],
      clip = "off", name = paste0("key-", k))
    gt <- gtable::gtable_add_grob(gt, labs[[k]], t = row[k], l = lab_l[k],
      clip = "off", name = paste0("label-", k))
  }
  
  # Wrap in a single-cell gtable, so legends with different layouts (vertical,
  # horizontal, with or without title) share one structure and can be stacked
  # with rbind()/cbind()
  out <- gtable::gtable(widths = sum(gt$widths), heights = sum(gt$heights))
  out <- gtable::gtable_add_grob(out, gt, t = 1, l = 1, clip = "off",
    name = "glyph-legend")
  class(out) <- c(class(out), "gspace_legend")
  out
}

#-------------------------------------------------------------------------------
# One key: a short shaft between two reference points, with the start glyph
# at the left and the end glyph at the right. The reference points are inset
# by half a glyph size, since some glyphs reach past their reference point.
.glyph_legend_key <- function(start, end, glyph_size, colour, linewidth) {
  inset <- grid::unit(0.5 * glyph_size, "mm")
  x0 <- inset
  x1 <- grid::unit(1, "npc") - inset
  # the shaft ends where each glyph says (its offset, in glyph units, one
  # glyph unit being glyph_size mm)
  off <- function(g) if (is.null(g) || is.null(g$offset)) 0 else g$offset
  s0 <- x0 + grid::unit(-off(start) * glyph_size, "mm")
  s1 <- x1 - grid::unit(-off(end) * glyph_size, "mm")
  s1 <- grid::unit.pmax(s0, s1)   # trims too long for the key: no shaft
  shaft <- grid::segmentsGrob(x0 = s0, x1 = s1, y0 = 0.5, y1 = 0.5,
    gp = ggplot2::gg_par(col = colour, lwd = linewidth, lty = "solid",
      lineend = "butt"))
  grid::gTree(children = grid::gList(shaft,
    .glyph_legend_end(start, x0, -1, glyph_size, colour, linewidth),
    .glyph_legend_end(end, x1, 1, glyph_size, colour, linewidth)))
}

#-------------------------------------------------------------------------------
# One glyph at a key end, pointing outward (ox = 1 right, ox = -1 left). The
# glyph is drawn in a square viewport of side `glyph_size` mm, centred on its
# reference point, so one npc unit equals one glyph unit on both axes. The
# grob is then built at once with the edge builders in .draw_primitives; no
# draw-time step (and no S3 method) is involved.
.glyph_legend_end <- function(glyph, x, ox, glyph_size, colour, linewidth) {
  if (is.null(glyph) || nrow(glyph$shape) == 0L) return(grid::nullGrob())
  vp <- grid::viewport(x = x, y = 0.5,
    width = grid::unit(glyph_size, "mm"), height = grid::unit(glyph_size, "mm"))
  piece <- list(draw = glyph$draw,
    p = .glyph_legend_place(glyph$shape, ox, colour, linewidth))
  build <- .draw_primitives[[glyph$draw]]$build
  grid::gTree(children = grid::gList(build(list(piece), "round", "mitre")),
    vp = vp)
}

#-------------------------------------------------------------------------------
# Place a shape at the centre of its square viewport, in npc, oriented along
# +x (ox = 1) or -x (ox = -1). Same rotation as .place_shape() with oy = 0,
# at unit size; returns the fields the builders read.
.glyph_legend_place <- function(shape, ox, colour, linewidth) {
  list(x = 0.5 + shape[, 1] * ox, y = 0.5 + shape[, 2] * ox,
    n = 1L, k = nrow(shape), asize = 1, col = colour, lwd = linewidth)
}
