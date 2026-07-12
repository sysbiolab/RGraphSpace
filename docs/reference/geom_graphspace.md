# Convenience wrapper for node and edge geoms

`geom_graphspace()` adds both node and edge layers to a ggplot2 plot by
calling
[`geom_nodespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md)
and
[`geom_edgespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)
in sequence. It is a convenience wrapper with no logic of its own; any
argument accepted by either underlying geom can be passed via
`node.params` or `edge.params`.

For independent control of node and edge layers, use
[`geom_nodespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md)
and
[`geom_edgespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)
directly.

## Usage

``` r
geom_graphspace(mapping = NULL, node.params = list(), edge.params = list())
```

## Arguments

- mapping:

  An optional [`aes`](https://ggplot2.tidyverse.org/reference/aes.html)
  call passed to
  [`geom_nodespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md).
  The most common use is supplying node label aesthetics, e.g.
  `aes(label = nodeLabel)`.

- node.params:

  A named list of additional arguments forwarded to
  [`geom_nodespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md).

- edge.params:

  A named list of additional arguments forwarded to
  [`geom_edgespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md).

## Value

A list of two ggplot2 layers, which ggplot2 flattens automatically when
added to a plot with `+`.

## See also

[`geom_nodespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md),
[`geom_edgespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md),
[`plotGraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/plotGraphSpace-methods.md)

## Examples

``` r
library(ggplot2)
data("gtoy1", package = "RGraphSpace")
gs <- GraphSpace(gtoy1)
#> Validating the 'igraph' object...
#> Ignoring graph-level attributes: 'name', 'mode', 'center'
#> Creating a 'GraphSpace' object...

# Simplest use
ggplot(gs) + geom_graphspace()


# With node labels
ggplot(gs) + geom_graphspace(aes(label = nodeLabel))


# With independent node and edge customization
ggplot(gs) + geom_graphspace(
  node.params = list(aes(label = nodeLabel)),
  edge.params = list(curve = 0.3)
)

```
