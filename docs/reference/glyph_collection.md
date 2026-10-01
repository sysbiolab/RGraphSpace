# Edge glyph prototypes

A collection of prototypes for the symbols drawn at an edge end (arrows,
bars, empty ends, ...). Each is a static, self-contained `gs_glyph`
object built with
[`glyph_proto`](https://sysbiolab.github.io/RGraphSpace/reference/glyph_proto.md):
a fixed shape in a canonical local frame (reference point at the origin,
`+x` outward along the edge, `+y` to its left, unit size), together with
the `arrowType` token(s) that select it. Glyphs carry no positioning,
size, or colour; those are edge attributes applied at render time (see
`arrow_size` in
[`geom_edgespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)).

## Usage

``` r
GlyphArrow

GlyphBar

GlyphNone

GlyphTriangle1

GlyphTriangle2

GlyphHarpoon1

GlyphHarpoon2

GlyphDiamond1

GlyphDiamond2

GlyphChevron1

GlyphChevron2

GlyphArrowBar1

GlyphArrowBar2

GlyphDoubleArrow1

GlyphDoubleArrow2

GlyphBarredArrow1

GlyphBarredArrow2

GlyphBlock1

GlyphBlock2

GlyphCircle1

GlyphCircle2

GlyphSquare1

GlyphSquare2

GlyphStar1

GlyphStar2

GlyphCross1

GlyphCross2

GlyphNotch1

GlyphNotch2

GlyphDoubleBar1

GlyphDoubleBar2

GlyphReverseArrow1

GlyphReverseArrow2
```

## Format

Objects of class `gs_glyph`.

## Details

The basic glyphs (group `"basic"`: arrow, bar, and no glyph) follow
common conventions for positive and negative effects. The arrow and bar
are also the primitives of the *vee-like* and *tee-like* extended
glyphs, grouped by the silhouette they form with the edge: `"vee-like"`
glyphs end in a point, and `"tee-like"` glyphs end in a wider shape. The
extended glyphs are numbered within their group (e.g. `">01"`, `"|03"`).
Most shapes come in pairs of consecutive numbers: a filled form (odd)
followed by its open form (even). The numbered glyphs carry no
predefined meaning; explain them with a legend (see
[`glyph_legend`](https://sysbiolab.github.io/RGraphSpace/reference/glyph_legend.md)).

These prototypes define RGraphSpace's built-in glyph vocabulary. They
are discovered automatically at package load: any `gs_glyph` object in
the package namespace becomes available through its declared token(s).
Use
[`glyph_list`](https://sysbiolab.github.io/RGraphSpace/reference/glyph_list.md)
to see the available tokens and
[`glyph_proto`](https://sysbiolab.github.io/RGraphSpace/reference/glyph_proto.md)
for how a new glyph is added.

## Adding a glyph

New glyphs are contributed by adding an exported `Glyph*` object to the
package's `gspace-glyph-collection.R` source file, which documents the
full recipe. There is no runtime registration.

## See also

[`glyph_proto`](https://sysbiolab.github.io/RGraphSpace/reference/glyph_proto.md),
[`glyph_list`](https://sysbiolab.github.io/RGraphSpace/reference/glyph_list.md),
[`geom_edgespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)

## Examples

``` r
GlyphArrow
#> <glyph 'arrow': 3 points, draw = polyline, token = >>
plot(GlyphArrow)

plot(GlyphArrow, GlyphTriangle1, GlyphDiamond1)

```
