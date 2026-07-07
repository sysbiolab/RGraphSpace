# Draw node and edge elements in a 2D graph layout

**\[deprecated\]**

Deprecated as of v1.4.2. Use
[`geom_edgespace()`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)`() + `[`geom_nodespace()`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md)
instead. These geoms support all current features including node-edge
synchronization, labels, multiple edges, and self-loops.

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
  arrow_size = 0.5,
  arrow_offset = 0.01,
  curve = 0,
  edge_spread = 0.2,
  loop_direction = "adaptive",
  raster = FALSE,
  dpi = NULL,
  dev = "cairo",
  scale = 1
)
```

## Arguments

- mapping, data, stat, position, na.rm, show.legend, inherit.aes:

  See
  [`geom_edgespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)
  and
  [`geom_nodespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md).

- ...:

  Additional arguments passed to the underlying geoms.

- arrow_size, arrow_offset, curve, edge_spread, loop_direction:

  See
  [`geom_edgespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md).

- raster, dpi, dev, scale:

  See
  [`geom_nodespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md).

## Value

A ggplot2 layer.

## Note

This function is deprecated. Replace `geom_graphspace(data = gs)` with
`geom_edgespace() + geom_nodespace()` in your `ggplot(gs)` call.

## See also

[geom_nodespace](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md),
[geom_edgespace](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)
