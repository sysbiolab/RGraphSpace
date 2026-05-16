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
  data = edgespace_handler(),
  stat = StatEdgeSpace,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  arrow_size = 1,
  arrow_offset = 0.01,
  lineend = "butt",
  linejoin = "mitre"
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
  object, or the `edgespace_handler()` handler (default).

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

  Numeric scaling factor controlling arrowhead geometry (see 'drawing'
  section).

- arrow_offset:

  Numeric value controlling the base offset of arrows at edge endpoints
  (see 'drawing' section).

- lineend:

  Line end style (round, butt, square). Supplied for compatibility with
  [geom_segment](https://ggplot2.tidyverse.org/reference/geom_segment.html).

- linejoin:

  Line join style (round, mitre, bevel). Supplied for compatibility with
  [geom_segment](https://ggplot2.tidyverse.org/reference/geom_segment.html).

## Value

A ggplot2 layer that renders edge segments defined by
[GeomEdgeSpace](https://sysbiolab.github.io/RGraphSpace/reference/GeomEdgeSpace.md).

## Details

**arrow_size** is a numeric scaling factor controlling arrowhead
geometry. The value is interpreted in the same numeric space as line
width (`lwd`), ensuring consistent scaling between edge strokes and
arrowheads.

**arrow_offset** is an additive term that offsets arrow endpoints
uniformly in graph space and is bounded by the edge length, in NPC
units.

Arrowhead types are specified in the
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
constructor.

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
| **`x`, `y`, `xend`, `yend`** | Required (automatically supplied). |
| `colour` | Node border colour (see [aes_colour_fill_alpha](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)). |
| `alpha` | Transparency (see [aes_colour_fill_alpha](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)). |
| `linetype` | Edge line type (see [aes_linetype_size_shape](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)). |
| `linewidth` | Edge line width (see [aes_linetype_size_shape](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)). |

Required aesthetics (`x`, `y`, `xend`, `yend`, ...) are supplied from
the
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object and do not need to be manually mapped.

Additional parameters can be passed to control fixed values for the
layer. For example: `colour = "grey"`, `linetype = 2`, `linewidth = 1`.

Arrows can be further adjusted by `arrow_size` and `arrow_offset`
arguments (see *details*).

## Integration with ggraph

`geom_nodespace` is compatible with the `ggraph` methods. When used
within a `ggraph()` call, the default `edgespace_handler()` handler
automatically:

- Identifies the current `layout_ggraph`.

- Extracts the `x` and `y` coordinates calculated by `ggraph`.

- Reconstructs a temporary `GraphSpace` object to inject spatial
  metadata and user-chosen `ggraph` layout.

## See also

[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md),
[geom_nodespace](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md),
[geom_graphspace](https://sysbiolab.github.io/RGraphSpace/reference/geom_graphspace.md),
[geom_segment](https://ggplot2.tidyverse.org/reference/geom_segment.html)

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
#> Creating a 'GraphSpace' object...

if (FALSE) { # \dontrun{

ggplot() +
  geom_edgespace(data = gs) +
  geom_nodespace(data = gs) +
  theme(aspect.ratio = 1)

} # }
```
