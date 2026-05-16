# GeomNodeSpace: a ggplot2 prototype for GraphSpace-class methods

`GeomNodeSpace` is the underlying
[ggproto](https://ggplot2.tidyverse.org/reference/ggproto.html) object
used by
[geom_nodespace](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md)
to draw node elements in a graph layout.

This geom is designed for network diagrams, where graph attributes are
often already in their final form (e.g., hex colors).

## Usage

``` r
GeomNodeSpace
```

## Aesthetics

`GeomNodeSpace` understands ggplot2's conventions for point-like geoms.

## See also

[geom_nodespace](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md),
[geom_point](https://ggplot2.tidyverse.org/reference/geom_point.html)
