# Build a new glyph prototype

Create a `gs_glyph` object from a static shape and its identifying
token. These prototypes form the building blocks of RGraphSpace's glyph
vocabulary (e.g.
[`GlyphArrow`](https://sysbiolab.github.io/RGraphSpace/reference/glyph_collection.md)).

## Usage

``` r
glyph_proto(
  shape,
  token,
  name = NULL,
  draw = c("polyline", "segments", "polygon", "circle"),
  offset = 0
)

# S3 method for class 'gs_glyph'
plot(x, ..., ncol = NULL, margin = 0.05, colour = "black")
```

## Arguments

- shape:

  A two-column numeric matrix of local points in the canonical frame:
  column 1 is the coordinate along the edge (reference point at the
  origin, `+x` outward), column 2 is the lateral coordinate (`+y` to the
  left), at unit size. An empty (0-row) matrix draws nothing.

- token:

  The `arrowType` token that selects this glyph: `">"` (arrow) or `"|"`
  (terminal), followed by a two-digit number (e.g. `">90"`, `"|90"`).

- name:

  A short human-readable name shown by
  [`glyph_list`](https://sysbiolab.github.io/RGraphSpace/reference/glyph_list.md).
  Defaults to the token when `NULL`.

- draw:

  How the points are interpreted: `"polyline"` connects them tip-to-tail
  into one open line; `"segments"` pairs consecutive points (rows 1-2,
  3-4, ...) into separate segments and requires an even number of rows;
  `"polygon"` connects them into a closed, filled outline; `"circle"`
  takes a single point as the centre, drawn at the edge's `arrow_size`
  diameter.

- offset:

  Where the edge line ends, as an x position in the glyph's frame: `0`
  (the default) runs the line to the reference point, and a negative
  value stops it that far back along the edge, in glyph units. Use it
  for open outlines, so the line stops at the outline instead of
  crossing it (e.g. `-1` for an open triangle whose base is at x = -1).

- x:

  A `gs_glyph` object, as returned by `glyph_proto()` or a built-in
  glyph (e.g. `GlyphArrow`).

- ...:

  Further `gs_glyph` objects, drawn side by side with `x`.

- ncol:

  Number of glyphs per row; defaults to a single row.

- margin:

  Space around the glyphs, as a fraction of the page on each

- colour:

  Colour used to draw the glyph preview.

## Value

A `gs_glyph` object.

## New glyphs

New glyphs are added as package contributions, not at runtime: add an
exported `Glyph*` object, built with `glyph_proto()`, to the
`gspace-glyph-collection.R` source file (see that file for the full
recipe). Tokens must be unique across all glyphs; conflicts are reported
at package load.

## See also

[`GlyphArrow`](https://sysbiolab.github.io/RGraphSpace/reference/glyph_collection.md),
[`glyph_list`](https://sysbiolab.github.io/RGraphSpace/reference/glyph_list.md)

## Examples

``` r
# a pair of arrows: a filled triangle (odd number) and its open form, the
# same outline as a closed polyline (next, even number)
m <- rbind(c(0, 0), c(-1, 0.6), c(-1, -0.6))
filled <- glyph_proto(m, token = ">91", draw = "polygon")
m <- rbind(c(-1, 0), c(-1, 0.6), c(0, 0), c(-1, -0.6), c(-1, 0))
open <- glyph_proto(m, token = ">92", draw = "polyline")
plot(filled, open)


# a pair of terminals: a filled block across the edge at the reference
# point, and its open form, the same outline as a closed polyline
m <- rbind(c(0, 0.65), c(0, -0.65), c(-0.25, -0.65), c(-0.25, 0.65))
filled <- glyph_proto(m, token = "|91", draw = "polygon")
m <- rbind(c(-0.25, 0), c(-0.25, 0.65), c(0, 0.65), c(0, -0.65),
  c(-0.25, -0.65), c(-0.25, 0))
open <- glyph_proto(m, token = "|92", draw = "polyline")
plot(filled, open)


# a pair of terminals: a circle (diameter one unit) touching the reference
# point, and its open form, a ring traced as a polyline
filled <- glyph_proto(rbind(c(-0.5, 0)), token = "|93", draw = "circle")
a <- seq(0, 2 * pi, length.out = 49)
m <- cbind(-0.5 - 0.5 * cos(a), 0.5 * sin(a))
open <- glyph_proto(m, token = "|94", draw = "polyline")
plot(filled, open)


# a single open glyph: a bar across the edge at the reference point, as
# one segment
m <- rbind(c(0, 0.65), c(0, -0.65))
glyph <- glyph_proto(m, token = "|95", draw = "segments")
plot(glyph)

```
