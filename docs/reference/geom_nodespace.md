# Draw node elements in a 2D graph layout

Constructor for
[GeomNodeSpace](https://sysbiolab.github.io/RGraphSpace/reference/GeomNodeSpace.md)
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
geom_nodespace(
  mapping = NULL,
  data = nodespace_handler(),
  stat = StatNodeSpace,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
)

nodespace_handler()
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
  object, or the `nodespace_handler()` handler (default).

- stat:

  The statistical transformation to use on the data. Defaults to
  `identity`.

- position:

  Position adjustment, either as a string or the result of a call to a
  position adjustment function.

- ...:

  Additional parameters passed to the underlying drawing function in
  [GeomNodeSpace](https://sysbiolab.github.io/RGraphSpace/reference/GeomNodeSpace.md).

- na.rm:

  Logical. Should missing values be removed? Defaults to `FALSE`.

- show.legend:

  Logical or a named logical vector indicating whether this layer should
  be included in legends.

- inherit.aes:

  Logical. If `FALSE` (default), the layer will use aesthetics defined
  in `mapping`.

## Value

A ggplot2 layer that renders node glyphs defined by
[GeomNodeSpace](https://sysbiolab.github.io/RGraphSpace/reference/GeomNodeSpace.md).

## Details

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

## Aesthetics

`geom_nodespace()` understands
[geom_point](https://ggplot2.tidyverse.org/reference/geom_point.html)
aesthetics.

If these aesthetics are not explicitly provided in
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html), they are
automatically retrieved from the
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object.

|  |  |
|----|----|
| **`x`, `y`** | Required (automatically supplied). |
| `fill` | Node interior colour (see [aes_colour_fill_alpha](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)). |
| `colour` | Node border colour (see [aes_colour_fill_alpha](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)). |
| `alpha` | Transparency (see [aes_colour_fill_alpha](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)). |
| `shape` | Node shape (see [points](https://rdrr.io/r/graphics/points.html) and [aes_linetype_size_shape](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)). |
| `size` | Node size (see *drawing* section and [aes_linetype_size_shape](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)). |
| `stroke` | Node line width (see [gg_par](https://ggplot2.tidyverse.org/reference/gg_par.html) and [aes_linetype_size_shape](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)). |

Required aesthetics `x` and `y` are supplied from the
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object and do not need to be manually mapped.

Additional parameters can be passed to control fixed values for the
layer. For example: `fill = "red"`, `stroke = 3`, `alpha = 0.5`, or
`shape = 21`.

## Integration with ggraph

`geom_nodespace` is compatible with the `ggraph` methods. When used
within a `ggraph()` call, the default `nodespace_handler()` handler
automatically:

- Identifies the current `layout_ggraph`.

- Extracts the `x` and `y` coordinates calculated by `ggraph`.

- Reconstructs a temporary `GraphSpace` object to inject spatial
  metadata and user-chosen `ggraph` layout.

## See also

[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md),
[geom_edgespace](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md),
[geom_graphspace](https://sysbiolab.github.io/RGraphSpace/reference/geom_graphspace.md),
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
geom_edgespace(data = gs, arrow_offset = 0.01) +
  geom_nodespace(mapping = aes(size = nodeSize, fill = user_var2), 
  data = gs) + 
  scale_size(range = c(1, 12)) + 
  theme(aspect.ratio = 1)
  
# Example 2: Nodes scaling with the viewport
# When 'size' is passed as a node attribute, 
# inherited from the igraph object, it is 
# interpreted as a percentage of the plotting 
# area and translated to NPC units.

ggplot() + 
geom_edgespace(data = gs, arrow_offset = 0.01) +
geom_nodespace(mapping = aes(fill = user_var2), data = gs) +
theme(aspect.ratio = 1)
  
} # }
```
