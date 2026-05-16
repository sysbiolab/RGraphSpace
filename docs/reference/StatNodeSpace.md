# Attribute Processing for GeomNodeSpace

Manage visual attribute precedence (color, size, shape) for
`GeomNodeSpace` objects.

## Usage

``` r
StatNodeSpace
```

## Format

A `ggproto` object.

## Attribute Priority

1.  Explicit [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html)
    mappings.

2.  Fixed
    [`geom_nodespace()`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md)
    arguments.

3.  Original graph attributes (via `optional_aes`).

During the `setup_data` stage, the Stat invokes internal functions to
resolve value priority:

1.  **Explicit Mapping**: Values defined by the user inside
    [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

2.  **Fixed Parameters**: Constant values passed as arguments in the
    [`geom_nodespace()`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md)
    call.

3.  **Graph Attributes**: Original attributes stored within the
    GraphSpace object, retrieved from the data columns.

## See also

[`geom_nodespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md)
