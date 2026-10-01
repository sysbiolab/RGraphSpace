################################################################################
### Adding a glyph
################################################################################
# A glyph is added in this file and nowhere else; there is no registry to
# edit. Each glyph is a prototype: an exported object of class `gs_glyph`,
# built once by glyph_proto() when the package is installed. At package load,
# every `gs_glyph` object in the namespace is discovered by its class and
# becomes usable in `arrowType` codes through its declared token(s).
#
# ------
# Recipe
# ------
# Add the object to the section of its kind, vee-like or tee-like (see the table
# below):
#
#   #' @rdname glyph_collection
#   #' @export
#   GlyphMyShape1 <- glyph_proto(
#     shape = rbind(c(0, 0), c(-1, 0.5), c(-1, -0.5)),
#     draw  = "polygon",
#     token = ">91",  # placeholder: use the next free number (see token)
#     name  = "my shape"
#   )
#
# then add a row for it to the table below.
#
# -----------
# Conventions
# -----------
# Name: CamelCase, prefixed with Glyph and suffixed with the form, like
#   ggplot2 prototypes (GeomPoint): GlyphMyShape1 for the filled form and
#   GlyphMyShape2 for the open form; both forms share one display name
#   (name = "my shape"). glyph_* names are reserved for functions
#   (glyph_proto(), glyph_list(), glyph_legend()). Discovery uses the class,
#   not the name, but the convention keeps objects and functions apart.
#
# shape: a two-column matrix in the canonical frame: the origin is the
#   reference point, where the edge meets the node; +x points along the edge
#   into the node; +y points to its left. Units are glyph units, scaled by
#   arrow_size at render time; the shape carries no size, colour or
#   orientation.
#   - Tip-anchor the shape: its forward-most point at x = 0 and the rest at
#     x <= 0, so the glyph ends at the node without entering it.
#     .tip_anchor() does this for a shape designed around its centre.
#   - Keep it at around one unit, and reuse the shared dimensions defined
#     below (.glyph_w head half-width, .glyph_h bar half-height, .glyph_g
#     bar gap, .glyph_b block depth), so it matches the other glyphs.
#   - The edge line is drawn up to the origin, so it runs through open
#     shapes (e.g. the open diamond); design them with that line in mind.
#
# draw: how the points are drawn. Glyphs are drawn with the edge's colour and
#   line width, with round line ends and mitre joins.
#   "polyline"  one open line through the points
#   "polygon"   closed outline, filled with the edge colour (>= 3 points)
#   "segments"  separate segments from point pairs 1-2, 3-4, ... (even
#               number of points)
#   "circle"    one point, the centre; the diameter is one unit, so a centre
#               at x = -0.5 makes the circle touch the node
#
# token: ">" for a vee-like or "|" for a tee-like, followed by a two-digit
#   number. Numbers are permanent: once released, a number is never reused
#   or reassigned. Filled glyphs take odd numbers and open glyphs even
#   numbers. A filled glyph takes the next free odd number, and its open
#   form the following even number (e.g. ">01" triangle, ">02" open
#   triangle); a filled glyph must come with its open form. A glyph with
#   only an open form takes the next free even number, leaving the odd
#   number before it free for a filled counterpart. The single symbols ">",
#   "|" and "-" are the basic glyphs.
#
# name: a short display name, shown by glyph_list() and when the glyph is
#   printed.
#
# offset: where the edge line ends, as an x position in the glyph frame
#   (default 0, the reference point). Set it for open outlines the line would
#   otherwise cross, to the point where the line enters the outline (e.g. -1
#   for an open triangle whose base is at x = -1).
#
# group: not declared; glyph_proto() derives it from the token: "basic" for
#   a single symbol, "vee-like" for ">" and a number, "tee-like" for "|" and a
#   number. glyph_list() lists each group in number order.
#
# ---------
# Check it
# ---------
# Preview a shape before adding it (plot() fits the shape to its box):
#
#   plot(glyph_proto(rbind(c(0, 0), c(-1, 0.5), c(-1, -0.5)),
#     token = ">91", draw = "polygon"))
#
# In a local clone of the repository, after adding the glyph:
#
#   devtools::load_all()   # rebuilds the collection and the vocabulary
#   glyph_list()           # the new token should be listed
#   devtools::test()
#
# The package protects itself: glyph_proto() validates as it builds, so a
# malformed glyph stops the package from loading or installing; a token
# declared by two glyphs stops package load; and the tests check that every
# glyph is valid, exported, and reaches the vocabulary. Then open a pull
# request.
#
# Maintainer note: glyphs are built when this file is loaded, so it must
# come after gspace-glyph-constructor.R in the DESCRIPTION Collate field.

################################################################################
################################################################################
### The RGraphSpace glyph collection starts here
################################################################################
################################################################################
# Tokens: ">" vee-like or "|" tee-like, then the glyph's number; a filled form
# (odd) is followed by its open form (even). In codes, a start token is
# written mirrored, e.g. "01<->01" (triangles at both ends) or "04|-|04"
# (rings at both ends).
#
# Group 'basic':
#                >        arrow           vee-like primitive
#                |        bar             tee-like primitive
#                -        none            no glyph
# Group 'vee-like':
#                >01 >02  triangles      (filled and open)
#                >03 >04  harpoons       (filled and open)
#                >05 >06  diamonds       (filled and open)
#                >07 >08  chevrons       (filled and open)
#                >09 >10  arrow bars     (filled and open)
#                >11 >12  double arrows  (filled and open)
#                >13 >14  barred arrows  (filled and open)
# Group 'tee-like':
#                |01 |02  blocks         (filled and open)
#                |03 |04  circles        (filled and open)
#                |05 |06  squares        (filled and open)
#                |07 |08  stars          (filled and open)
#                |09 |10  crosses        (filled and open)
#                |11 |12  notches        (filled and open)
#                |13 |14  double bars    (filled and open)
#                |15 |16  reverse arrows (filled and open)

#-------------------------------------------------------------------------------
# Shared dimensions and helpers (not glyphs); defined before the first glyph,
# since the glyph objects are built when this file is loaded
#-------------------------------------------------------------------------------
.glyph_w <- tan(30 * pi / 180) # head half-width (30 degree half-opening angle)
.glyph_h <- 0.65               # bar half-height
.glyph_g <- 0.4                # gap between a head and its extra bar
.glyph_b <- 0.25               # block depth (kept well below .glyph_g)

# Tip-anchor a shape: shift it back along x until its forward-most point
# sits at x = 0
.tip_anchor <- function(m) {
  m[, 1] <- m[, 1] - max(m[, 1])
  m
}

################################################################################
### Group 'basic': 'vee-like' and 'tee-like' primitives, and no glyph
################################################################################

#' @rdname glyph_collection
#' @export
GlyphArrow <- glyph_proto(
  shape = rbind(c(-1, .glyph_w), c(0, 0), c(-1, -.glyph_w)),
  draw  = "polyline",
  token = ">",
  name = "arrow"
)

#' @rdname glyph_collection
#' @export
GlyphBar <- glyph_proto(
  shape = rbind(c(0, .glyph_h), c(0, -.glyph_h)),
  draw  = "segments",
  token = "|",
  name = "bar"
)

#' @rdname glyph_collection
#' @export
GlyphNone <- glyph_proto(
  shape = matrix(numeric(0), nrow = 0, ncol = 2),
  draw  = "polyline",
  token = "-",
  name = "none"
)

################################################################################
### Group 'vee-like': pairs of filled (odd) and open (even) forms
################################################################################

#-------------------------------------------------------------------------------
# Triangle

#' @rdname glyph_collection
#' @export
GlyphTriangle1 <- glyph_proto(
  shape = rbind(c(0, 0), c(-1, .glyph_w), c(-1, -.glyph_w)),
  draw  = "polygon",
  token = ">01",
  name = "triangle"
)

#' @rdname glyph_collection
#' @export
GlyphTriangle2 <- glyph_proto(
  # Outline of GlyphTriangle1
  shape = rbind(
    c(-1, 0), 
    c(-1, .glyph_w),
    c(0, 0), 
    c(-1, -.glyph_w),
    c(-1, 0)),
  draw  = "polyline",
  token = ">02",
  offset = -1,
  name = "triangle"
)

#-------------------------------------------------------------------------------
# Harpoons

#' @rdname glyph_collection
#' @export
GlyphHarpoon1 <- glyph_proto(
  shape = rbind(c(0, 0), c(-1, .glyph_w), c(-1, 0)),
  draw  = "polygon",
  token = ">03",
  name = "harpoon"
)

#' @rdname glyph_collection
#' @export
GlyphHarpoon2 <- glyph_proto(
  shape = rbind(c(-1, .glyph_w), c(0, 0)),
  draw  = "polyline",
  token = ">04",
  name = "harpoon"
)

#-------------------------------------------------------------------------------
# Diamond

#' @rdname glyph_collection
#' @export
GlyphDiamond1 <- glyph_proto(
  shape = .tip_anchor(rbind(c(0, 1), c(1, 0), 
    c(0, -1), c(-1, 0)) * .glyph_h),
  draw  = "polygon",
  token = ">05",
  name = "diamond"
)

#' @rdname glyph_collection
#' @export
GlyphDiamond2 <- glyph_proto(
  # Half-diagonal 0.65, the same extent as GlyphDiamond1
  shape = .tip_anchor(rbind(c(-1, 0), c(0, 1), 
    c(1, 0), c(0, -1), c(-1, 0)) * .glyph_h),
  draw  = "polyline",
  token = ">06",
  offset = -1.3,
  name = "diamond"
)

#-------------------------------------------------------------------------------
# Chevrons

#' @rdname glyph_collection
#' @export
GlyphChevron1 <- glyph_proto(
  shape = rbind(
    c(-1, .glyph_w), c(0, 0), 
    c(-1, -.glyph_w), c(-0.6, 0)),
  draw  = "polygon",
  token = ">07",
  name = "chevron"
)

#' @rdname glyph_collection
#' @export
GlyphChevron2 <- glyph_proto(
  # Outline of GlyphChevron1, starting and ending at the notch, where the
  # edge line enters
  shape = rbind(
    c(-0.6, 0), 
    c(-1, .glyph_w), 
    c(0, 0), 
    c(-1, -.glyph_w),
    c(-0.6, 0)),
  draw  = "polyline",
  token = ">08",
  offset = -0.6,
  name = "chevron"
)

#-------------------------------------------------------------------------------
# Arrow bars

#' @rdname glyph_collection
#' @export
GlyphArrowBar1 <- glyph_proto(
  shape = rbind(
    c(0, .glyph_h), 
    c(0, -.glyph_h), 
    c(-.glyph_b, -.glyph_h),
    c(-.glyph_b, 0), 
    c(-1, -.glyph_w), 
    c(-1, .glyph_w), 
    c(-.glyph_b, 0),
    c(-.glyph_b, .glyph_h)),
  draw  = "polygon",
  token = ">09",
  name  = "arrow bar"
)

#' @rdname glyph_collection
#' @export
GlyphArrowBar2 <- glyph_proto(
  shape = rbind(
    c(0, .glyph_h), c(0, -.glyph_h),
    c(0, 0), c(-1, .glyph_w),
    c(0, 0), c(-1, -.glyph_w)),
  draw  = "segments",
  token = ">10",
  name  = "arrow bar"
)

#-------------------------------------------------------------------------------
# Double arrows

#' @rdname glyph_collection
#' @export
GlyphDoubleArrow1 <- glyph_proto(
  shape = rbind(
    c(0, 0), 
    c(-0.7, .glyph_w), c(-0.7, 0),
    c(-1.4, .glyph_w), c(-1.4, -.glyph_w),
    c(-0.7, 0), c(-0.7, -.glyph_w)),
  draw  = "polygon",
  token = ">11",
  name = "double arrow"
)

#' @rdname glyph_collection
#' @export
GlyphDoubleArrow2 <- glyph_proto(
  shape = rbind(
    c(0, 0), c(-0.7, .glyph_w), 
    c(0, 0), c(-0.7, -.glyph_w),
    c(-0.5, 0), c(-1.2, .glyph_w), 
    c(-0.5, 0), c(-1.2, -.glyph_w)),
  draw  = "segments",
  token = ">12",
  name = "double arrow"
)

#-------------------------------------------------------------------------------
# Barred arrows

#' @rdname glyph_collection
#' @export
GlyphBarredArrow1 <- glyph_proto(
  shape = rbind(
    c(0, 0), c(-1, .glyph_w), c(-1, 0),
    c(-1 - .glyph_g, 0), c(-1 - .glyph_g, .glyph_h),
    c(-1 - .glyph_g - .glyph_b, .glyph_h),
    c(-1 - .glyph_g - .glyph_b, -.glyph_h),
    c(-1 - .glyph_g, -.glyph_h), c(-1 - .glyph_g, 0),
    c(-1, 0), c(-1, -.glyph_w)),
  draw  = "polygon",
  token = ">13",
  name = "barred arrow"
)

#' @rdname glyph_collection
#' @export
GlyphBarredArrow2 <- glyph_proto(
  shape = rbind(
    c(0, 0), c(-1, .glyph_w), 
    c(0, 0), c(-1, -.glyph_w),
    c(-1 - .glyph_g, .glyph_h), 
    c(-1 - .glyph_g, -.glyph_h)),
  draw  = "segments",
  token = ">14",
  name = "barred arrow"
)

################################################################################
### Group 'tee-like': pairs of filled (odd) and open (even) forms
################################################################################

#-------------------------------------------------------------------------------
# Block

#' @rdname glyph_collection
#' @export
GlyphBlock1 <- glyph_proto(
  shape = rbind(
    c(0, .glyph_h), 
    c(0, -.glyph_h), 
    c(-.glyph_b, -.glyph_h),
    c(-.glyph_b, .glyph_h)),
  draw  = "polygon",
  token = "|01",
  name = "block"
)

#' @rdname glyph_collection
#' @export
GlyphBlock2 <- glyph_proto(
  shape = rbind(
    c(-.glyph_b, 0), 
    c(-.glyph_b, .glyph_h),
    c(0, .glyph_h),
    c(0, -.glyph_h), 
    c(-.glyph_b, -.glyph_h), 
    c(-.glyph_b, 0)),
  draw  = "polyline",
  token = "|02",
  offset = -.glyph_b,
  name = "block"
)

#-------------------------------------------------------------------------------
# Circle

#' @rdname glyph_collection
#' @export
GlyphCircle1 <- glyph_proto(
  # Centre at x = -0.5 makes the circle touch the reference point
  shape = rbind(c(-0.5, 0)),
  draw  = "circle",
  token = "|03",
  name = "circle"
)

#' @rdname glyph_collection
#' @export
GlyphCircle2 <- glyph_proto(
  # Ring drawn as a closed polyline (48 segments), touching the origin
  shape = cbind(-0.5 - 0.5 * cos(seq(0, 2 * pi, length.out = 49)),
    0.5 * sin(seq(0, 2 * pi, length.out = 49))),
  draw  = "polyline",
  token = "|04",
  offset = -1,
  name = "circle"
)

#-------------------------------------------------------------------------------
# Square

#' @rdname glyph_collection
#' @export
GlyphSquare1 <- glyph_proto(
  # Same area as GlyphDiamond1: half-side = its half-diagonal / sqrt(2)
  shape = .tip_anchor(rbind(c(-1, 1), c(1, 1), c(1, -1), c(-1, -1)) *
      0.65 / sqrt(2)),
  draw  = "polygon",
  token = "|05",
  name = "square"
)

#' @rdname glyph_collection
#' @export
GlyphSquare2 <- glyph_proto(
  # Outline of GlyphSquare1
  shape = .tip_anchor(rbind(c(-1, 0), c(-1, 1), c(1, 1), c(1, -1),
    c(-1, -1), c(-1, 0)) * 0.65 / sqrt(2)),
  draw  = "polyline",
  token = "|06",
  offset = -0.65 * sqrt(2),
  name = "square"
)

#-------------------------------------------------------------------------------
# Star

# A five-pointed star with one point at the origin (pointing into the node),
# outer radius 0.6 and inner radius 0.3.
.star <- function(open = FALSE) {
  k <- if (open) c(5:9, 0:5) else 0:9
  a <- k * pi / 5
  r <- ifelse(k %% 2 == 0, 0.6, 0.3)
  .tip_anchor(cbind(r * cos(a), r * sin(a)))
}

#' @rdname glyph_collection
#' @export
GlyphStar1 <- glyph_proto(
  shape = .star(),
  draw  = "polygon",
  token = "|07",
  name = "star"
)

#' @rdname glyph_collection
#' @export
GlyphStar2 <- glyph_proto(
  shape = .star(open = TRUE),
  draw  = "polyline",
  token = "|08",
  offset = -0.9,
  name = "star"
)

#-------------------------------------------------------------------------------
# Cross

# A thick X: a plus sign with arms of half-width k, rotated 45 degrees and
# tip-anchored in a 1 x 1 box
.cross_thick <- function(k) {
  L <- 0.5 * sqrt(2) - k
  plus <- rbind(c(k, L), c(k, k), c(L, k), c(L, -k), c(k, -k), c(k, -L),
    c(-k, -L), c(-k, -k), c(-L, -k), c(-L, k), c(-k, k), c(-k, L))
  r <- sqrt(0.5)
  x <- cbind(r * (plus[, 1] - plus[, 2]), r * (plus[, 1] + plus[, 2]))
  .tip_anchor(x)
}

#' @rdname glyph_collection
#' @export
GlyphCross1 <- glyph_proto(
  # A thick X: a plus sign with arms 0.12 wide, rotated 45 degrees and fit
  # to the same 1 x 1 box as GlyphCross2
  shape = .cross_thick(0.12),
  draw  = "polygon",
  token = "|09",
  name = "cross"
)

#' @rdname glyph_collection
#' @export
GlyphCross2 <- glyph_proto(
  shape = rbind(
    c(0, 0.5), 
    c(-1, -0.5), 
    c(0, -0.5), 
    c(-1, 0.5)),
  draw  = "segments",
  token = "|10",
  name = "cross"
)

#-------------------------------------------------------------------------------
# Notch

#' @rdname glyph_collection
#' @export
GlyphNotch1 <- glyph_proto(
  shape = .tip_anchor(rbind(
    c(0, 0), c(0, .glyph_h), c(0.5, .glyph_h), c(0, 0),
    c(0.5, -.glyph_h), c(0, -.glyph_h), c(0, 0))),
  draw  = "polygon",
  token = "|11",
  offset = -0.5,
  name = "notch"
)

#' @rdname glyph_collection
#' @export
GlyphNotch2 <- glyph_proto(
  shape = .tip_anchor(rbind(
    c(0, 0), c(0, .glyph_h), c(0.5, .glyph_h), c(0, 0),
    c(0.5, -.glyph_h), c(0, -.glyph_h), c(0, 0))),
  draw  = "polyline",
  token = "|12",
  offset = -0.5,
  name  = "notch"
)

#-------------------------------------------------------------------------------
# Double bar 

#' @rdname glyph_collection
#' @export
GlyphDoubleBar1 <- glyph_proto(
  # Two blocks 0.15 deep, the back one at gap .glyph_g behind the front one,
  # traced as one outline: the blocks are joined by a zero-width bridge
  # along the edge axis, which the edge line covers
  shape = rbind(
    c(0, .glyph_h), c(0, -.glyph_h), c(-0.15, -.glyph_h), c(-0.15, 0),
    c(-.glyph_g, 0), c(-.glyph_g, -.glyph_h),
    c(-.glyph_g - 0.15, -.glyph_h), c(-.glyph_g - 0.15, .glyph_h),
    c(-.glyph_g, .glyph_h), c(-.glyph_g, 0),
    c(-0.15, 0), c(-0.15, .glyph_h)),
  draw  = "polygon",
  token = "|13",
  name = "double bar"
)

#' @rdname glyph_collection
#' @export
GlyphDoubleBar2 <- glyph_proto(
  shape = rbind(
    c(0, .glyph_h), 
    c(0, -.glyph_h),
    c(-.glyph_g, .glyph_h), 
    c(-.glyph_g, -.glyph_h)),
  draw  = "segments",
  token = "|14",
  name = "double bar"
)

#-------------------------------------------------------------------------------
# Reverse arrow

#' @rdname glyph_collection
#' @export
GlyphReverseArrow1 <- glyph_proto(
  # A filled head pointing back along the edge: its point on the axis, its
  # base across the edge at the node
  shape = .tip_anchor(rbind(
    c(.glyph_h, -.glyph_h), c(0, 0), c(.glyph_h, .glyph_h))),
  draw  = "polygon",
  token = "|15",
  name  = "reverse arrow"
)

#' @rdname glyph_collection
#' @export
GlyphReverseArrow2 <- glyph_proto(
  # A V opening towards the node, its point on the edge axis
  shape = .tip_anchor(rbind(
    c(.glyph_h, -.glyph_h), c(0, 0), c(.glyph_h, .glyph_h))),
  draw  = "polyline",
  token = "|16",
  offset = -.glyph_h,
  name  = "reverse arrow"
)
