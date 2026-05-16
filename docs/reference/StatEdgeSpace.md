# Attribute Processing for GeomEdgeSpace

Manage visual attribute precedence (color, size, shape) for
`GeomEdgeSpace` objects.

## Usage

``` r
StatEdgeSpace
```

## Format

A `ggproto` object.

## Attribute Priority

1.  Explicit [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html)
    mappings.

2.  Fixed
    [`geom_edgespace()`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)
    arguments.

3.  Original graph attributes (via `optional_aes`).

During the `setup_data` stage, the Stat invokes internal functions to
resolve value priority:

1.  **Explicit Mapping**: Values defined by the user inside
    [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

2.  **Fixed Parameters**: Constant values passed as arguments in the
    [`geom_edgespace()`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)
    call.

3.  **Graph Attributes**: Original attributes stored within the
    GraphSpace object, retrieved from the data columns.

## See also

[`geom_edgespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)
