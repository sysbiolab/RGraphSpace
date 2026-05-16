# GeomEdgeSpace: a ggplot2 prototype for GraphSpace-class methods

`GeomEdgeSpace` is the underlying
[ggproto](https://ggplot2.tidyverse.org/reference/ggproto.html) object
used by
[geom_edgespace](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)
to draw edge elements in a graph layout.

This geom is designed for network diagrams, where graph attributes are
often already in their final form (e.g., hex colors).

## Usage

``` r
GeomEdgeSpace
```

## Aesthetics

`GeomEdgeSpace` understands ggplot2's conventions for segment-like
geoms.

## See also

[geom_edgespace](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md),
[geom_segment](https://ggplot2.tidyverse.org/reference/geom_segment.html)
