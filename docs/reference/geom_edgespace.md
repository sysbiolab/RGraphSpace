# Draw edge elements in a 2D graph layout

Constructor for
[GeomEdgeSpace](https://sysbiolab.github.io/RGraphSpace/reference/GeomEdgeSpace.md)
ggproto objects.

A wrapper around
[geom_segment](https://ggplot2.tidyverse.org/reference/geom_segment.html)
that enables direct use of edge attributes stored in
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
objects as aesthetics.

This `geom` is designed to create edge-level aesthetics such as `colour`
and `linewidth`, or any custom aesthetics defined in
[GeomEdgeSpace](https://sysbiolab.github.io/RGraphSpace/reference/GeomEdgeSpace.md).

## Usage

``` r
geom_edgespace(
  mapping = NULL,
  data = NULL,
  stat = StatEdgeSpace,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  arrow_size = 0.5,
  arrow_offset = 0.01,
  curve = 0,
  parallel_spread = 1,
  loop_direction = "adaptive",
  lineend = "butt",
  linejoin = "mitre",
  raster = FALSE,
  dpi = NULL,
  dev = "cairo",
  scale = 1
)

edgespace_handler()
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  These mappings override global aesthetics and are not inherited from
  the top-level plot.

- data:

  The data to be displayed in this layer. It can be a
  [GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
  object, an
  [igraph](https://r.igraph.org/reference/aaa-igraph-package.html)
  object, or the `edgespace_handler()` closure. When `NULL` (default), a
  handler is created internally.

- stat:

  The statistical transformation to use on the data. Defaults to
  `identity`.

- position:

  Position adjustment, either as a string or the result of a call to a
  position adjustment function.

- ...:

  Additional parameters passed to the underlying drawing function in
  [GeomEdgeSpace](https://sysbiolab.github.io/RGraphSpace/reference/GeomEdgeSpace.md).

- na.rm:

  Logical. Should missing values be removed? Defaults to `FALSE`.

- show.legend:

  Logical or a named logical vector indicating whether this layer should
  be included in legends.

- inherit.aes:

  Logical. If `FALSE` (default), the layer will use aesthetics defined
  in `mapping`.

- arrow_size:

  Numeric scaling factor controlling arrowhead geometry (see 'details').

- arrow_offset:

  Numeric value controlling the base offset of arrows at edge endpoints
  (see 'details').

- curve:

  Numeric. Controls edge curvature, as a fraction of edge length.
  Non-zero values bow the edge into a smooth curve, and the sign
  controls which side it bows toward. Ignored for loops and parallel
  edges (see 'details').

- parallel_spread:

  Controls the lateral spread of parallel edges and self-loops. Ignored
  for simple non-loop edges (see 'details').

- loop_direction:

  Controls how self-loops are oriented around their node. Options:
  `'adaptive'` (default), `'opposite'`, and an angle in degrees (see
  'details').

- lineend:

  Line end style (`'round'`, `'butt'`, `'square'`). Supplied for
  compatibility with
  [geom_segment](https://ggplot2.tidyverse.org/reference/geom_segment.html).

- linejoin:

  Line join style (`'round'`, `'mitre'`, `'bevel'`). Supplied for
  compatibility with
  [geom_segment](https://ggplot2.tidyverse.org/reference/geom_segment.html).

- raster:

  Logical. Should node glyphs be rasterized? Rasterization support is
  based on
  [`rasterise`](https://rdrr.io/pkg/ggrastr/man/rasterise.html).

- dpi:

  Numeric. Rasterization resolution.

- dev:

  Character. Rasterization backend. One of `'cairo'`, `'ragg'`,
  `'ragg_png'`, or `'cairo_png'`.

- scale:

  Numeric. Rasterization scaling factor (see
  [`rasterise`](https://rdrr.io/pkg/ggrastr/man/rasterise.html)).

## Value

A ggplot2 layer that renders edge segments defined by
[GeomEdgeSpace](https://sysbiolab.github.io/RGraphSpace/reference/GeomEdgeSpace.md).

## Details

**arrow_size** is a numeric scaling factor controlling arrowhead
geometry. The value is interpreted in the same numeric space as line
width (`lwd`).

**arrow_offset** is an additive term that offsets arrow endpoints
uniformly in graph space and is bounded by the edge length, in NPC
units.

Arrowhead types are specified in the
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
constructor.

**curve** bows an edge through a control point displaced perpendicular
to the edge, by `curve` times the edge length. `curve = 0` (default)
renders a straight edge. Typical visible values range from about 0.1 to
0.4; sign sets which side the edge bows toward.

**parallel_spread** controls the fan opening for parallel edges,
reciprocal `A->B`/`B->A` pairs, and self-loops – anything where multiple
edges share the same vertex pair. `curve` has no effect on these edges;
`parallel_spread` governs both their curvature magnitude and how far
apart they fan. A value of `0` collapses all edges in a group onto the
same position; increasing values progressively open the fan. Self-loops
behave the same way: a single loop uses `parallel_spread` to set its own
size, and multiple loops at the same node fan out accordingly. A
built-in minimum, tied to `arrow_size` and node size, keeps small
`parallel_spread` values from producing a loop whose arrowhead looks
skewed against its own curvature.

**loop_direction** determines where self-loops sit relative to their
node. `"adaptive"` (default) points each loop in the direction that
faces away from the graph's centroid. `"opposite"` is a two-sided
arrangement: loops are split into two groups placed above and below the
node. A numeric angle (in degrees) places all loops at a fixed direction
regardless of their node's position in the layout. When node position
data is unavailable, `"adaptive"` silently falls back to `"opposite"`.

## Aesthetics

`geom_edgespace()` understands
[geom_segment](https://ggplot2.tidyverse.org/reference/geom_segment.html)
aesthetics.

If these aesthetics are not explicitly provided in
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html), they are
automatically retrieved from the
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object.

|  |  |
|----|----|
| **`x`, `y`, `xend`, `yend`** | Required; automatically supplied. |
| `colour` | Edge colour (see [aes_colour_fill_alpha](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)). |
| `alpha` | Transparency (see [aes_colour_fill_alpha](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)). |
| `linetype` | Edge line type (see [aes_linetype_size_shape](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)). |
| `linewidth` | Edge line width (see [aes_linetype_size_shape](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)). |

Required aesthetics (`x`, `y`, `xend`, `yend`) are supplied from the
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object and do not need to be manually mapped.

Additional parameters can be passed to control fixed values for the
layer. For example: `colour = "grey"`, `linetype = 2`, `linewidth = 1`.

Arrows can be further adjusted by `arrow_size` and `arrow_offset`
arguments (see *details*).

## Label aesthetics

When `label` is mapped via
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html), a text
label is drawn at the visual midpoint of each edge. Labels follow the
rendered edge geometry: the chord midpoint for straight edges, the
Bezier midpoint for curved edges, and the loop apex for self-loops.
Edges with `NA` labels are silently skipped.

The `label_colour` aesthetic defaults to the edge `colour`, and
`label_alpha` defaults to the edge `alpha`. All other `label_*`
aesthetics default to
[`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html)
when not set.

|  |  |
|----|----|
| **`label`** | Required to activate label rendering. |
| `label_colour` | Label text colour (see [`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html)). |
| `label_alpha` | Transparency (see [`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html)). |
| `label_fill` | Background colour (see [`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html)). |
| `label_size` | Font size (see [`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html)). |
| `label_angle` | Rotation angle (see [`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html)). |
| `label_hjust` | Horizontal justification (see [`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html)). |
| `label_vjust` | Vertical justification (see [`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html)). |
| `label_lwd` | Border linewidth (see [`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html)). |
| `label_lty` | Border linetype (see [`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html)). |
| `label_family` | Font family (see [`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html)). |
| `label_fontface` | Font face (see [`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html)). |
| `label_lineheight` | Line height (see [`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html)). |

## See also

[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md),
[geom_nodespace](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md),
[geom_graphspace](https://sysbiolab.github.io/RGraphSpace/reference/geom_graphspace.md),
[geom_segment](https://ggplot2.tidyverse.org/reference/geom_segment.html),
[geom_label](https://ggplot2.tidyverse.org/reference/geom_text.html)

## Examples

``` r
library(RGraphSpace)
library(igraph)
library(ggplot2)

# Load a demo igraph
data('gtoy1', package = 'RGraphSpace')

# Create a GraphSpace object
gs <- GraphSpace(gtoy1)
#> Validating the 'igraph' object...
#> Ignoring graph-level attributes: 'name', 'mode', 'center'
#> Creating a 'GraphSpace' object...

if (FALSE) { # \dontrun{

ggplot(gs) +
  geom_edgespace() +
  geom_nodespace() +
  theme(aspect.ratio = 1)

} # }
```
