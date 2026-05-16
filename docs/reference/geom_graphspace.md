# Draw node and edge elements in a 2D graph layout

Constructor for
[GeomGraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GeomGraphSpace.md)
ggproto objects.

A wrapper around
[geom_point](https://ggplot2.tidyverse.org/reference/geom_point.html)
that enables direct use of node attributes stored in
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
objects as aesthetics.

This geom is designed to map node-level attributes (e.g., `fill`,
`size`) or any aesthetics supported by
[GeomPoint](https://ggplot2.tidyverse.org/reference/Geom.html).

## Usage

``` r
geom_graphspace(
  mapping = NULL,
  data,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  arrow_size = 1,
  arrow_offset = 0.01
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  These mappings override global aesthetics and are not inherited from
  the top-level plot.

- data:

  A
  [GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
  object.

- stat:

  The statistical transformation to use on the data. Defaults to
  `identity`.

- position:

  Position adjustment, either as a string or the result of a call to a
  position adjustment function.

- ...:

  Additional parameters passed to the underlying drawing function in
  [GeomGraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GeomGraphSpace.md).

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

## Value

A ggplot2 layer that renders node glyphs defined by
[GeomGraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GeomGraphSpace.md).

## Aesthetics for node drawing

Nodes are drawn in the main layer of `geom_graphspace()`, which
understands
[geom_point](https://ggplot2.tidyverse.org/reference/geom_point.html)
aesthetics.

If these aesthetics are not explicitly provided in
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html), they are
automatically retrieved from the
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object.

|  |  |
|----|----|
| **`x`, `y`, `vertex`** | Required (automatically supplied). |
| `fill` | Node interior colour (see [aes_colour_fill_alpha](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)). |
| `colour` | Node border colour (see [aes_colour_fill_alpha](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)). |
| `alpha` | Transparency (see [aes_colour_fill_alpha](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)). |
| `shape` | Node shape (see [points](https://rdrr.io/r/graphics/points.html) and [aes_linetype_size_shape](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)). |
| `size` | Node size (see *drawing* section and [aes_linetype_size_shape](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)). |
| `stroke` | Node line width (see [gg_par](https://ggplot2.tidyverse.org/reference/gg_par.html) and [aes_linetype_size_shape](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)). |

Required aesthetics `x`, `y`, and `vertex` are supplied from the
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object and do not need to be manually mapped.

Additional parameters can be passed to control fixed values for the
layer. For example: `fill = "red"`, `stroke = 3`, `alpha = 0.5`, or
`shape = 21`.

The interpretation of **`size`** depends on how it is provided:

- **As an aesthetic**: When mapped within
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html), `size`
  follows the behavior of
  [geom_point](https://ggplot2.tidyverse.org/reference/geom_point.html),
  using absolute units to ensure consistency with the plot legends.

- **As a parameter**: When set outside
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html), `size` is
  treated as a percentage of the viewport (`[0, 100]`), scaling in `npc`
  units. This allows nodes to resize dynamically with viewport changes.

## Edge context-aware parameters

These parameters control the edge appearance. If not explicitly
provided, they are automatically retrieved from the
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object. They can be a single value or a vector matching the number of
edges:

|                  |                     |
|------------------|---------------------|
| `edge_colour`    | Node border colour. |
| `edge_linetype`  | Edge line type.     |
| `edge_linewidth` | Edge line width.    |
| `edge_alpha`     | Edge transparency.  |

## Edge global parameters

These parameters apply globally to all edges in the layer:

|  |  |
|----|----|
| `arrow_size` | Arrow scaling factor (default = 1). |
| `arrow_offset` | Arrow offset from nodes (default = 0.01). |
| `arrow_lineend` | Line end style (see [gpar](https://rdrr.io/r/grid/gpar.html)). |
| `arrow_linejoin` | Line join style (see [gpar](https://rdrr.io/r/grid/gpar.html)). |

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

## See also

[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md),
[geom_nodespace](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md),
[geom_edgespace](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md),
[geom_point](https://ggplot2.tidyverse.org/reference/geom_point.html)

## Examples

``` r
library(RGraphSpace)
library(igraph)
library(ggplot2)

# Make a demo igraph
gtoy1 <- make_star(15, mode="out")

# Set some node attributes
V(gtoy1)$nodeSize <- runif(vcount(gtoy1), 1, 20)
V(gtoy1)$nodeColor <- rainbow(vcount(gtoy1))

# Set some variables
V(gtoy1)$user_var1 <- runif(vcount(gtoy1), 1, 3)^3
V(gtoy1)$user_var2 <-  rep(c(1, 2, 3), each = 5)

# Create a GraphSpace object
gs <- GraphSpace(gtoy1, layout = layout_in_circle(gtoy1))
#> Validating the 'igraph' object...
#> Vertex attribute 'name' missing; assigning names... 
#> Creating a 'GraphSpace' object...

if (FALSE) { # \dontrun{

# Example 1: Nodes scaling with the legend
# When 'size' is mapped inside aes(), it follows
# ggplot2 default behavior: size is translated 
# to absolute units (mm) via 'scale_size()'.

ggplot() + 
  geom_graphspace(
  mapping = aes(size = nodeSize, fill = user_var2), 
  data = gs, arrow_offset = 0.01) + 
  scale_size(range = c(1, 12)) + 
  theme(aspect.ratio = 1)
  
# Example 2: Nodes scaling with the viewport
# When 'size' is passed as a node attribute, 
# inherited from the igraph object, it is
# interpreted as a percentage of the plotting 
# area and translated to NPC units.

ggplot() +
  geom_graphspace(mapping = aes(fill = user_var2), 
  data = gs, arrow_offset = 0.01) + 
  theme(aspect.ratio = 1)
  
} # }
```
