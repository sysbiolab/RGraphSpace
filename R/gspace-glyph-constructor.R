#-------------------------------------------------------------------------------
#' Edge glyph prototypes
#' 
#' A collection of prototypes for the symbols drawn at an edge end 
#' (arrows, bars, empty ends, ...). Each is a static, self-contained 
#' \code{gs_glyph} object built with \code{\link{glyph_proto}}: a fixed 
#' shape in a canonical local frame (reference point at the origin,
#' \code{+x} outward along the edge, \code{+y} to its left, unit size), 
#' together with the \code{arrowType} token(s) that select it. Glyphs 
#' carry no positioning, size, or colour; those are edge attributes 
#' applied at render time (see \code{arrow_size} in 
#' \code{\link{geom_edgespace}}).
#'
#' @details
#' 
#' The basic glyphs (group \code{"basic"}: arrow, bar, and no glyph) follow
#' common conventions for positive and negative effects. The arrow and bar
#' are also the primitives of the \emph{vee-like} and \emph{tee-like} extended
#' glyphs, grouped by the silhouette they form with the edge: \code{"vee-like"} 
#' glyphs end in a point, and \code{"tee-like"} glyphs end in a wider shape. 
#' The extended glyphs are numbered within their group (e.g. \code{">01"}, 
#' \code{"|03"}). Most shapes come in pairs of consecutive numbers: a filled 
#' form (odd) followed by its open form (even). The numbered glyphs carry no 
#' predefined meaning; explain them with a legend 
#' (see \code{\link{glyph_legend}}).
#' 
#' These prototypes define RGraphSpace's built-in glyph vocabulary. They 
#' are discovered automatically at package load: any \code{gs_glyph} object
#' in the package namespace becomes available through its declared token(s).
#' Use \code{\link{glyph_list}} to see the available tokens and
#' \code{\link{glyph_proto}} for how a new glyph is added.
#' 
#' @section Adding a glyph:
#' 
#' New glyphs are contributed by adding an exported \code{Glyph*} object
#' to the package's \code{gspace-glyph-collection.R} source
#' file, which documents the full recipe. There is no runtime registration.
#' 
#' @format Objects of class \code{gs_glyph}.
#'
#' @examples
#' GlyphArrow
#' plot(GlyphArrow)
#' plot(GlyphArrow, GlyphTriangle1, GlyphDiamond1)
#' 
#' @seealso \code{\link{glyph_proto}}, \code{\link{glyph_list}},
#'   \code{\link{geom_edgespace}}
#' @name glyph_collection
NULL

#-------------------------------------------------------------------------------
#' Build a new glyph prototype
#'
#' Create a \code{gs_glyph} object from a static shape and its identifying 
#' token. These prototypes form the building blocks of RGraphSpace's glyph
#' vocabulary (e.g. \code{\link{GlyphArrow}}).
#'
#' @param shape A two-column numeric matrix of local points in the canonical
#' frame: column 1 is the coordinate along the edge (reference point at the
#' origin, \code{+x} outward), column 2 is the lateral coordinate (\code{+y}
#' to the left), at unit size. An empty (0-row) matrix draws nothing.
#' @param token The \code{arrowType} token that selects this glyph:
#' \code{">"} (vee-like) or \code{"|"} (tee-like), followed by a
#' two-digit number (e.g. \code{">90"}, \code{"|90"}).
#' @param name A short human-readable name shown by \code{\link{glyph_list}}.
#' Defaults to the token when \code{NULL}.
#' @param draw How the points are interpreted:
#' \code{"polyline"} connects them tip-to-tail into one open line;
#' \code{"segments"} pairs consecutive points (rows 1-2, 3-4, ...) into
#' separate segments and requires an even number of rows;
#' \code{"polygon"} connects them into a closed, filled outline;
#' \code{"circle"} takes a single point as the centre, drawn at the edge's
#' \code{arrow_size} diameter.
#' @param offset Where the edge line ends, as an x position in the glyph's
#' frame: \code{0} (the default) runs the line to the reference point, and a
#' negative value stops it that far back along the edge, in glyph units. Use
#' it for open outlines, so the line stops at the outline instead of crossing
#' it (e.g. \code{-1} for an open triangle whose base is at x = -1).
#' @section New glyphs: 
#' 
#' New glyphs are added as package contributions, not at runtime: add an
#' exported \code{Glyph*} object, built with \code{glyph_proto()}, to
#' the \code{gspace-glyph-collection.R} source file (see that file for the full
#' recipe). Tokens must be unique across all glyphs; conflicts are reported at
#' package load.
#' 
#' @return A \code{gs_glyph} object.
#'
#' @examples
#' # a pair of vee-like shapes: a filled triangle (odd number) and its open
#' # form, the same outline as a closed polyline (next, even number)
#' m <- rbind(c(0, 0), c(-1, 0.6), c(-1, -0.6))
#' filled <- glyph_proto(m, token = ">91", draw = "polygon")
#' m <- rbind(c(-1, 0), c(-1, 0.6), c(0, 0), c(-1, -0.6), c(-1, 0))
#' open <- glyph_proto(m, token = ">92", draw = "polyline")
#' plot(filled, open)
#'
#' # a pair of tee-like shapes: a filled block across the edge at the reference
#' # point, and its open form, the same outline as a closed polyline
#' m <- rbind(c(0, 0.65), c(0, -0.65), c(-0.25, -0.65), c(-0.25, 0.65))
#' filled <- glyph_proto(m, token = "|91", draw = "polygon")
#' m <- rbind(c(-0.25, 0), c(-0.25, 0.65), c(0, 0.65), c(0, -0.65),
#'   c(-0.25, -0.65), c(-0.25, 0))
#' open <- glyph_proto(m, token = "|92", draw = "polyline")
#' plot(filled, open)
#'
#' # a pair of tee-like shapes: a circle (diameter one unit) touching the
#' # reference point, and its open form, a ring traced as a polyline
#' filled <- glyph_proto(rbind(c(-0.5, 0)), token = "|93", draw = "circle")
#' a <- seq(0, 2 * pi, length.out = 49)
#' m <- cbind(-0.5 - 0.5 * cos(a), 0.5 * sin(a))
#' open <- glyph_proto(m, token = "|94", draw = "polyline")
#' plot(filled, open)
#'
#' # a single open glyph: a bar across the edge at the reference point, as
#' # one segment
#' m <- rbind(c(0, 0.65), c(0, -0.65))
#' glyph <- glyph_proto(m, token = "|95", draw = "segments")
#' plot(glyph)
#'
#' @seealso \code{\link{GlyphArrow}}, \code{\link{glyph_list}}
#' @export
glyph_proto <- function(shape, token, name = NULL, 
  draw = c("polyline", "segments", "polygon", "circle"), 
  offset = 0) {
  
  if (is.data.frame(shape)) shape <- as.matrix(shape)
  if (missing(token)) rlang::abort("A glyph must declare a `token`.")
  if (is.null(name)) name <- token
  draw <- rlang::arg_match(draw)
  
  glyph <- structure(list(shape = shape, draw = draw, 
    token = token, offset = offset, name = name, 
    group = .token_group(token)),
    class = "gs_glyph")
  .validate_gs_glyph(glyph)
  glyph
}

#' @export
print.gs_glyph <- function(x, ...) {
  cat(sprintf("<glyph '%s': %d points, draw = %s, token = %s>\n",
    x$name, nrow(x$shape), x$draw, paste(x$token, collapse = "/")))
  invisible(x)
}

#' @rdname glyph_proto
#' @param x A \code{gs_glyph} object, as returned by \code{glyph_proto()} or
#' a built-in glyph (e.g. \code{GlyphArrow}).
#' @param ncol Number of glyphs per row; defaults to a single row.
#' @param margin Space around the glyphs, as a fraction of the page on each
#' @param colour Colour used to draw the glyph preview.
#' @param ... Further \code{gs_glyph} objects, drawn side by side with
#' \code{x}.
#' @export
plot.gs_glyph <- function(x, ..., ncol = NULL, margin = 0.05,
  colour = "black") {
  
  # Glyphs to draw: x, plus any glyphs passed in ...
  glyphs <- c(list(x), Filter(function(g) inherits(g, "gs_glyph"), list(...)))
  n <- length(glyphs)
  
  # One square cell per glyph, in rows of ncol (default: a single row),
  # inside a margin around the page
  if (is.null(ncol)) ncol <- n
  nrow <- ceiling(n / ncol)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(
    width = grid::unit(1 - 2 * margin, "npc"),
    height = grid::unit(1 - 2 * margin, "npc"),
    layout = grid::grid.layout(nrow, ncol)))
  on.exit(grid::popViewport())
  
  for (i in seq_len(n)) {
    grid::pushViewport(grid::viewport(
      layout.pos.row = (i - 1L) %/% ncol + 1L,
      layout.pos.col = (i - 1L) %% ncol + 1L))
    grid::pushViewport(grid::viewport(width = grid::unit(1, "snpc"),
      height = grid::unit(1, "snpc")))
    .draw_glyph_preview(glyphs[[i]], colour)
    grid::popViewport(2)
  }
  
  invisible(x)
}

# Draw one glyph, fitted to the current (square) viewport
.draw_glyph_preview <- function(g, colour = "black") {
  shape <- g$shape
  draw  <- g$draw
  if (nrow(shape) == 0L) return(invisible(NULL))
  
  # Place the glyph in the unit square, outward = +x. A circle is drawn at
  # diameter = asize (the builder draws r = asize / 2) and centred on its
  # point; other shapes are fit to their box.
  if (draw == "circle") {
    asize <- 0.8
    cx <- 0.5 - shape[1, 1] * asize
    cy <- 0.5 - shape[1, 2] * asize
  } else {
    span <- max(diff(range(shape[, 1])), diff(range(shape[, 2])), 1e-8)
    asize <- 0.8 / span
    cx <- 0.5 - mean(range(shape[, 1])) * asize
    cy <- 0.5 - mean(range(shape[, 2])) * asize
  }
  
  place <- data.frame(x = cx, y = cy, ox = 1, oy = 0, asize = asize,
    colour = colour, linewidth = 1, stringsAsFactors = FALSE)
  piece <- list(draw = draw, p = .place_shape(shape, place, sz2npc = 1))
  
  # Render with the same grob builders the edge renderer uses (table dispatch).
  build <- .draw_primitives[[draw]]$build
  grid::grid.draw(build(list(piece), "round", "mitre"))
  invisible(NULL)
}


#-------------------------------------------------------------------------------
# R has no built-in validity system for S3, so this is the constructor/validator
# idiom: the single definition of a valid glyph.
.validate_gs_glyph <- function(g) {
  if (!inherits(g, "gs_glyph") || !is.list(g)) {
    rlang::abort("a glyph must be a 'gs_glyph' list.")
  }
  if (!all(c("shape", "draw", "token", "name", "group", "offset") %in%
      names(g))) {
    rlang::abort(paste("a glyph must have `shape`, `draw`, `token`, `name`,",
      "`group`, and `offset`."))
  }
  if (!is.character(g$draw) || length(g$draw) != 1L ||
      !g$draw %in% names(.draw_primitives)) {
    rlang::abort(sprintf("`draw` must be one of %s.",
      paste(sprintf("'%s'", names(.draw_primitives)), collapse = ", ")))
  }
  .check_glyph_shape(g$shape, g$draw)
  .check_glyph_token(g$token)
  if (!is.character(g$name) || length(g$name) != 1L || 
      is.na(g$name) || !nzchar(g$name)) {
    rlang::abort("`name` must be a single non-empty string.")
  }
  if (!is.numeric(g$offset) || length(g$offset) != 1L ||
      !is.finite(g$offset) || g$offset > 0) {
    rlang::abort(paste("`offset` must be a single number, 0 or negative",
      "(how far back along the edge its line ends)."))
  }
  if (!identical(g$group, .token_group(g$token))) {
    rlang::abort(sprintf("`group` must be '%s', as given by the token '%s'.",
      .token_group(g$token), g$token))
  }
  invisible(g)
}

# The group a token belongs to: a single symbol is a basic glyph; otherwise
# the symbol gives the kind ('>' vee-like, '|' tee-like)
.token_group <- function(token) {
  # an invalid token has no group; .check_glyph_token() reports it
  if (!is.character(token) || length(token) != 1L || is.na(token)) {
    return(NA_character_)
  }
  if (nchar(token) == 1L) return("basic")
  if (substr(token, 1L, 1L) == ">") "vee-like" else "tee-like"
}

#-------------------------------------------------------------------------------
# Validate a shape against its primitive's contract (.draw_primitives).
.check_glyph_shape <- function(shape, draw) {
  if (!is.matrix(shape) || !is.numeric(shape) || ncol(shape) != 2L) {
    rlang::abort("'shape' must be a two-column numeric matrix.")
  }
  if (anyNA(shape)) {
    rlang::abort("'shape' must not contain missing values.")
  }
  n <- nrow(shape)
  spec <- .draw_primitives[[draw]]
  if (!is.na(spec$exact)) {
    if (n != spec$exact) {
      rlang::abort(sprintf(
        "A '%s' glyph needs exactly %d point(s); got %d.", draw, spec$exact, n))
    }
  } else {
    if (spec$even && n %% 2L != 0L) {
      rlang::abort(c(
        x = sprintf(
          "A '%s' glyph needs an even number of points; got %d.", draw, n),
        i = "Points pair up consecutively (rows 1-2, 3-4, ...)."
      ))
    }
    if (n > 0L && n < spec$min_points) {
      rlang::abort(sprintf(
        "A '%s' glyph needs at least %d points; got %d.",
        draw, spec$min_points, n))
    }
  }
  invisible(shape)
}

#-------------------------------------------------------------------------------
# Validate the declared token(s).
.check_glyph_token <- function(token) {
  if (!is.character(token) || length(token) != 1L || is.na(token)) {
    rlang::abort("`token` must be a single string.")
  }
  # A token is ">" (vee-like) or "|" (tee-like), optionally followed by a
  # two-digit number, or "-" (no glyph)
  if (!grepl("^(-|[>|]([0-9]{2})?)$", token)) {
    rlang::abort(c(sprintf("Invalid token '%s'.", token),
      i = paste("A token is '>' (vee-like) or '|' (tee-like), optionally",
        "followed by a two-digit number, or '-' for no glyph.")))
  }
  if (grepl("00$", token)) {
    rlang::abort(sprintf(paste("Invalid token '%s': number 00 is reserved",
      "for the basic glyphs ('>' and '|')."), token))
  }
  invisible(token)
}

################################################################################
### Glyph vocabulary: a fixed token -> glyph map, discovered from the exported
### Glyph* objects at package load. There is a single category of glyph:
### everything is built in. The vocabulary is never mutated at runtime, so a
### GraphSpace using any token is portable across sessions and machines.
################################################################################

.gspace_glyph_cache <- new.env(parent = emptyenv())

# Scan a namespace for gs_glyph objects and fold their declared tokens into a
# token -> glyph list. Nothing is called: membership is decided by class, so
# glyph_* functions (glyph_list(), glyph_legend(), ...) are never involved.
# Glyph objects are validated when built (at install), and again here; an
# invalid object is skipped, and the test suite turns that into a release
# blocker. A token declared by two glyphs is a hard error (an ambiguous
# vocabulary).
.discover_glyphs <- function(ns = topenv(environment())) {
  vocab <- list()
  for (nm in ls(ns, all.names = TRUE)) {
    g <- get(nm, envir = ns)
    if (!inherits(g, "gs_glyph")) next
    ok <- tryCatch({ .validate_gs_glyph(g); TRUE }, error = function(e) FALSE)
    if (!ok) next
    for (tk in g$token) {
      if (!is.null(vocab[[tk]])) {
        rlang::abort(sprintf(
          "Glyph token '%s' is declared by more than one glyph.", tk))
      }
      vocab[[tk]] <- g
    }
  }
  vocab
}

# The vocabulary, built once and cached. Populated at load by .onLoad(); the
# lazy fallback here keeps it correct if reached before .onLoad has run.
.glyph_vocab <- function() {
  v <- .gspace_glyph_cache$vocab
  if (is.null(v)) {
    v <- .discover_glyphs()
    .gspace_glyph_cache$vocab <- v
  }
  v
}

################################################################################
### glyph grobs
################################################################################
#-------------------------------------------------------------------------------
# Build vee-like/tee-like glyphs for both ends of every edge. Glyphs are
# batched by draw primitives: all polylines render as ONE polylineGrob, all polygons
# as ONE polygonGrob, all segments as ONE segmentsGrob, and all circles as ONE
# circleGrob, so the grob count is at most four regardless of edge count. The
# per-primitive builder is looked up in .draw_primitives.
.get_glyph_grobs <- function(edges, sz2npc, 
  lineend = "round", linejoin = "mitre") {
  
  pieces <- c(.place_pieces(edges, "start", sz2npc),
    .place_pieces(edges, "end", sz2npc))
  
  if (length(pieces) == 0) return(list())
  
  grobs <- lapply(names(.draw_primitives), function(draw) {
    pcs <- .pieces_with_draw(pieces, draw)
    if (length(pcs) == 0) return(NULL)
    .draw_primitives[[draw]]$build(pcs, lineend, linejoin)
  })
  grobs[!vapply(grobs, is.null, logical(1))]
}

#-------------------------------------------------------------------------------
# A side frame -> a list of "pieces", one per drawn glyph token. Rows whose
# token is "-" (no glyph) are dropped. Each piece holds the placed coordinates
# plus the per-instance colour/linewidth.
.place_pieces <- function(edges, which, sz2npc) {
  
  side <- .side_frame(edges, which)
  
  side <- side[side$token != "-", , drop = FALSE]
  
  if (nrow(side) == 0) return(list())
  
  vocab <- .glyph_vocab()
  lapply(unique(side$token), function(tk) {
    glyph <- vocab[[tk]] %||% vocab[["-"]]
    place <- side[side$token == tk, , drop = FALSE]
    p <- .place_shape(glyph$shape, place, sz2npc)
    list(draw = glyph$draw, p = p)
  })
  
}

#-------------------------------------------------------------------------------
# How far each edge line should stop short of its glyph at one end (start or
# end), in npc: the glyph's offset (glyph units) scaled by the glyph size and
# converted as in .place_shape(), so the line ends exactly where the glyph
# says, at any arrow_size. Returns 0 where the glyph has no offset.
.glyph_line_trim <- function(edges, which, sz2npc) {
  tokens <- if (which == "start") edges$arrowTokenStart else edges$arrowTokenEnd
  # look up each distinct token once, then spread to the edges
  u <- unique(tokens)
  vocab <- .glyph_vocab()
  off_u <- vapply(u, function(tk) {
    g <- vocab[[tk]]
    if (is.null(g) || is.null(g$offset)) 0 else g$offset
  }, numeric(1), USE.NAMES = FALSE)
  if (!any(off_u != 0)) return(numeric(length(tokens)))
  
  -off_u[match(tokens, u)] * edges$arrow_size * sz2npc
}

#-------------------------------------------------------------------------------
# Place a static shape using per-edge attributes: reference point (x, y) in npc,
# oriented by the outward vector (ox, oy), scaled by `asize` glyph units of
# 'sz2npc' (see .size_to_npc()).
# Vectorised over edges, edge-major: each edge's k points are consecutive.
# Returns the placed x/y plus per-instance n, k, asize, col, and lwd.
.place_shape <- function(shape, place, sz2npc) {
  
  k <- nrow(shape); n <- nrow(place)
  u <- rep(shape[, 1], times = n) * rep(place$asize, each = k)
  v <- rep(shape[, 2], times = n) * rep(place$asize, each = k)
  ox <- rep(place$ox, each = k); oy <- rep(place$oy, each = k)
  x0 <- rep(place$x,  each = k); y0 <- rep(place$y,  each = k)
  
  x <- x0 + (u * ox + v * (-oy)) * sz2npc
  y <- y0 + (u * oy + v * ( ox)) * sz2npc
  
  # Wrap up other geometry attributes for packing
  asize <- place$asize * sz2npc
  col <- place$colour
  lwd <- place$linewidth
  
  list(x = x, y = y, n = n, k = k, asize = asize, 
    col = col, lwd = lwd)
}

#-------------------------------------------------------------------------------
# edges -> a canonical "side" data frame for the start or end of each edge:
# columns x, y (the glyph reference point), ox, oy (the outward unit vector),
# asize, token, colour, linewidth. The outward vector is the edge tangent,
# negated at the start end so both point away from the node.
.side_frame <- function(edges, which) {
  if (which == "start"){
    data.frame(x = edges$x, y = edges$y,
      ox = -edges$px0, oy = -edges$py0,
      token = edges$arrowTokenStart, asize = edges$arrow_size,
      colour = edges$colour, linewidth = edges$linewidth,
      stringsAsFactors = FALSE)
  } else {
    data.frame(x = edges$xend, y = edges$yend,
      ox = edges$px1,  oy = edges$py1,
      token = edges$arrowTokenEnd, asize = edges$arrow_size, 
      colour = edges$colour, linewidth = edges$linewidth,
      stringsAsFactors = FALSE)
  }
}

#-------------------------------------------------------------------------------
# Keep the pieces drawn with a given draw primitive.
.pieces_with_draw <- function(pieces, draw) {
  keep <- vapply(pieces, function(pc) pc$draw == draw, logical(1))
  pieces[keep]
}

#-------------------------------------------------------------------------------
# All polyline pieces -> ONE polylineGrob. Coordinates concatenated; a global
# per-glyph-instance id keeps each glyph a separate line.
.polyline_from_pieces <- function(pieces, lineend, linejoin) {
  
  gp <- .pieces_gp(pieces, lineend, linejoin)
  x <- unlist(lapply(pieces, function(pc) pc$p$x))
  y <- unlist(lapply(pieces, function(pc) pc$p$y))
  ns <- vapply(pieces, function(pc) pc$p$n, integer(1))
  offs <- cumsum(c(0L, ns[-length(ns)]))
  ids <- unlist(Map(function(pc, o) rep(o + seq_len(pc$p$n),
    each = pc$p$k), pieces, offs))
  
  grid::polylineGrob(x = x, y = y,
    id = ids, gp = gp)
}

#-------------------------------------------------------------------------------
# All segment pieces -> ONE segmentsGrob. Within each piece the shape's points
# pair up consecutively (odd = start, even = end). segmentsGrob has no id
# grouping, so a k-point glyph is parts(k) = k/2 independently styled segments;
# the per-instance colour/width is expanded to one value per segment.
.segments_from_pieces <- function(pieces, lineend, linejoin) {
  
  x <- unlist(lapply(pieces, function(pc) pc$p$x))
  y <- unlist(lapply(pieces, function(pc) pc$p$y))
  nseg <- .draw_primitives$segments$parts   # points -> segments per instance
  col <- unlist(lapply(pieces, function(pc) rep(pc$p$col, each = nseg(pc$p$k))))
  lwd <- unlist(lapply(pieces, function(pc) rep(pc$p$lwd, each = nseg(pc$p$k))))
  
  gp <- ggplot2::gg_par(col = col, fill = col, lwd = lwd,
    lty = "solid", lineend = lineend, linejoin = linejoin)
  
  starts <- seq(1, length(x), by = 2)
  ends <- seq(2, length(x), by = 2)
  grid::segmentsGrob(x0 = x[starts], y0 = y[starts],
    x1 = x[ends], y1 = y[ends], gp = gp)
}

#-------------------------------------------------------------------------------
# All polygon pieces -> ONE polygonGrob. Coordinates concatenated; a global
# per-glyph-instance id keeps each glyph a separate polygon.
.polygon_from_pieces <- function(pieces, lineend, linejoin) {
  
  gp <- .pieces_gp(pieces, lineend, linejoin)
  x <- unlist(lapply(pieces, function(pc) pc$p$x))
  y <- unlist(lapply(pieces, function(pc) pc$p$y))
  ns <- vapply(pieces, function(pc) pc$p$n, integer(1))
  offs <- cumsum(c(0L, ns[-length(ns)]))
  ids <- unlist(Map(function(pc, o) rep(o + seq_len(pc$p$n),
    each = pc$p$k), pieces, offs))
  grid::polygonGrob(x = x, y = y,
    id = ids, gp = gp)
}

#-------------------------------------------------------------------------------
# All circle pieces -> ONE circleGrob. Each piece represents one glyph.
.circle_from_pieces <- function(pieces, lineend, linejoin) {
  
  gp <- .pieces_gp(pieces, lineend, linejoin)
  x <- unlist(lapply(pieces, function(pc) pc$p$x))
  y <- unlist(lapply(pieces, function(pc) pc$p$y))
  r <- unlist(lapply(pieces, function(pc) pc$p$asize/2))
  
  grid::circleGrob(
    x = x, y = y, r = r,
    gp = gp
  )
}

#-------------------------------------------------------------------------------
# One gpar for a set of pieces: one colour/linewidth per glyph instance. Correct
# for id-grouped grobs (polyline, polygon) and circles (one point each); the
# segments builder expands per segment itself (see .segments_from_pieces).
.pieces_gp <- function(pieces, lineend, linejoin) {
  col <- unlist(lapply(pieces, function(pc) pc$p$col))
  ggplot2::gg_par(
    col = col, fill = col,
    lwd = unlist(lapply(pieces, function(pc) pc$p$lwd)),
    lty = "solid", lineend = lineend, linejoin = linejoin)
}

#-------------------------------------------------------------------------------
# The contract for every draw primitive, in one place. A new primitive is added
# by adding a row here and writing its builder; validation (.check_glyph_shape),
# gp-cardinality (parts), grob dispatch (.get_glyph_grobs, plot.gs_glyph) and the
# set of accepted `draw` values all read from this table.
#   exact      : required point count, or NA
#   even       : point count must be even (ignored when `exact` is set)
#   min_points : minimum points when non-empty (ignored when `exact` is set)
#   parts      : points -> number of independently styled primitives per glyph
#   build      : pieces -> a single grob
# Order here is the grob draw (z) order in .get_glyph_grobs().
.draw_primitives <- list(
  polyline = list(exact = NA_integer_, even = FALSE, min_points = 0L,
    parts = function(k) 1L, build = .polyline_from_pieces),
  polygon  = list(exact = NA_integer_, even = FALSE, min_points = 3L,
    parts = function(k) 1L, build = .polygon_from_pieces),
  segments = list(exact = NA_integer_, even = TRUE,  min_points = 0L,
    parts = function(k) k %/% 2L,  build = .segments_from_pieces),
  circle   = list(exact = 1L, even = FALSE, min_points = 1L,
    parts = function(k) 1L, build = .circle_from_pieces)
)

################################################################################
### Internal accessors
################################################################################

#-------------------------------------------------------------------------------
# arrowType -> matrix of c(start, end) tokens per edge. Accepts integer codes
# (numbers, or strings such as "-1" in a character vector, so the levels can
# be mixed) and token codes in shafted form ("-->", "<-|", "01<->01"). A
# glyph left of the shaft is the start token, right of it the end token; "-",
# absent, or NA means "no glyph".
# at <- glyph_list()$token
.arrowtype_to_tokens <- function(at){
  if (is.numeric(at)) {
    # integer codes: map each to its basic token code
    at <- as.character(at)
    idx <- !at %in% names(.int_tokens)
    if(any(idx)) at[idx] <- names(.int_tokens)[1]
    # Resolve the distinct codes only, then expand by match()
    u <- unique(at)
    m <- do.call(rbind, .int_tokens[u])
    m[] <- .normalize_token(m)
    m <- m[match(at, u), , drop = FALSE]
    colnames(m) <- c("start", "end")
    return(m)
  }
  at <- as.character(at)
  parse1 <- function(s) {
    if (is.na(s) || s == "" || grepl("^-+$", s)){
      return(c("-", "-"))
    }
    # an integer code written as a string (e.g. "-1")
    if (grepl("^(0|-?[1-4])$", s)) return(.int_tokens[[s]])
    m <- regexpr("-+", s)
    pos <- as.integer(m)
    len <- attr(m, "match.length")
    if (pos < 1L){
      return(c("-", s))
    }
    left  <- substr(s, 1L, pos - 1L)
    right <- substr(s, pos + len, nchar(s))
    c(
      if (nzchar(left)) left else "-",
      if (nzchar(right)) right else "-"
    )
  }
  # Parse only the distinct arrowType codes (very low cardinality) 
  # and expand by match()
  u <- unique(at)
  m <- t(vapply(u, parse1, character(2)))
  m[] <- .normalize_token(m)
  m <- m[match(at, u), , drop = FALSE]
  colnames(m) <- c("start", "end"); rownames(m) <- NULL
  m
}

# Read a token in its defined form: a mirrored token (number first, as
# written at the start of a code, e.g. "01<" or "04|") is reversed; '<' is
# read as '>', since both spell the same arrow glyphs; and number 00 is read
# as the basic glyph (">00" -> ">", "|00" -> "|").
.normalize_token <- function(tk) {
  tk <- sub("^([0-9]{2})([<>|])$", "\\2\\1", tk)
  tk <- sub("^<", ">", tk)
  sub("^([>|])00$", "\\1", tk)
}

#-------------------------------------------------------------------------------
# Canonical arrowType is a code: start token + "-" shaft + end token, with the
# start token mirrored, e.g. "-->", "|-|", "01<->01", "04|-|04". A token is "-"
# (no glyph) or any glyph token (see glyph_list()).
.tokens_to_arrowtype <- function(start, end) {
  paste0(.mirror_token(start), "-", end)
}

# Write a start token as it reads at the start of a code: mirrored, with the
# number outermost and '<' for arrows (">01" -> "01<", "|04" -> "04|",
# ">" -> "<")
.mirror_token <- function(tk) {
  long <- nchar(tk) > 1L
  tk[long] <- paste0(substring(tk[long], 2L), substr(tk[long], 1L, 1L))
  chartr(">", "<", tk)
}

#-------------------------------------------------------------------------------
# Reverse a full arrowType code end to end (start <-> end), so the same
# glyphs are drawn when the edge is read in the opposite direction
# ("02<--" -> "-->02", "|->" -> "<-|", 1 -> 2). Unknown integers are kept.
.mirror_arrowtype <- function(at) {
  if (is.numeric(at)) {
    out <- unname(.int_mirror[as.character(at)])
    return(ifelse(is.na(out), at, out))
  }
  tk <- .arrowtype_to_tokens(at)
  .tokens_to_arrowtype(tk[, "end"], tk[, "start"])
}

# Integer code with start and end swapped (see .int_tokens):
# 1 <-> 2, 4 <-> -4, -1 <-> -2; 0, 3 and -3 are symmetric
.int_mirror <- c("0" = 0, "1" = 2, "2" = 1, "3" = 3, "4" = -4,
  "-1" = -2, "-2" = -1, "-3" = -3, "-4" = 4)

#-------------------------------------------------------------------------------
# Check tokens against the glyph vocabulary
.valid_tokens <- function(tk) {
  valid <- names(.glyph_vocab())
  unname(tk[, "start"] %in% valid & tk[, "end"] %in% valid)
}

# Explain why tokens are not in the vocabulary, pointing to what exists.
# Returns one message per token (at most `n`), for use in warnings/errors.
.explain_tokens <- function(tokens, n = 3L) {
  v <- .glyph_vocab()
  groups <- c(">" = "vee-like", "|" = "tee-like")
  label <- function(tk) sprintf("'%s' (%s)", tk, v[[tk]]$name)
  last <- function(kind) {
    k <- names(v)[substr(names(v), 1L, 1L) == kind & nchar(names(v)) == 3L]
    if (length(k)) max(k) else NA_character_
  }
  msgs <- vapply(utils::head(unique(tokens), n), function(tk) {
    if (grepl("^[>|][0-9]{2}$", tk)) {
      kind <- substr(tk, 1L, 1L)
      return(sprintf("'%s' is not defined: %s glyphs go from '%s01' to '%s'.",
        tk, groups[[kind]], kind, last(kind)))
    }
    if (grepl("^[0-9]{2}$", tk)) {
      cand <- intersect(paste0(names(groups), tk), names(v))
      if (length(cand)) {
        return(sprintf("'%s' needs a group symbol: did you mean %s?", tk,
          paste(vapply(cand, label, ""), collapse = " or ")))
      }
      return(
        sprintf(
          "'%s' needs a group symbol, '>' (vee-like) or '|' (tee-like).", 
          tk))
    }
    sprintf(paste("'%s' is not a token: use '>' (arrow) or '|' (bar),",
      "optionally followed by a two-digit number."), tk)
  }, character(1), USE.NAMES = FALSE)
  stats::setNames(msgs, rep("i", length(msgs)))
}

#-------------------------------------------------------------------------------
# Integer codes and their basic token codes (start, end)
.int_tokens <- list(
  "0"  = c("-", "-"),
  "1"  = c("-", ">"), "2"  = c("<", "-"),
  "3"  = c("<", ">"), "4"  = c("|", ">"),
  "-1" = c("-", "|"), "-2" = c("|", "-"),
  "-3" = c("|", "|"), "-4" = c("<", "|")
)

#-------------------------------------------------------------------------------
# Integer codes accepted per graph type
.int_arrowtypes <- function(is.dir = FALSE) {
  if (is.dir) {
    atypes <- c(-1, 0, 1)
  } else {
    atypes <- c(1, 2, 3, 4)
    atypes <- c(0, atypes, -atypes)
  }
  atypes
}
