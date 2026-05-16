# GeomGraphSpace: a ggplot2 prototype for GraphSpace-class methods

`GeomGraphSpace` is the underlying
[ggproto](https://ggplot2.tidyverse.org/reference/ggproto.html) object
used by
[geom_graphspace](https://sysbiolab.github.io/RGraphSpace/reference/geom_graphspace.md)
to draw node and edge elements in a graph layout.

This geom is designed for network diagrams, where graph attributes are
often already in their final form (e.g., hex colors).

## Usage

``` r
GeomGraphSpace
```

## Aesthetics

`GeomGraphSpace` understands ggplot2's conventions for point-like geoms.

## See also

[geom_graphspace](https://sysbiolab.github.io/RGraphSpace/reference/geom_graphspace.md),
[geom_point](https://ggplot2.tidyverse.org/reference/geom_point.html)
