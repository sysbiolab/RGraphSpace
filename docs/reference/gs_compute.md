# Apply an igraph function to the graph inside a GraphSpace

`gs_compute()` runs any igraph function on the graph carried by a
`GraphSpace`, without needing a dedicated `gs_*` wrapper for each one.
It extracts the underlying igraph via
[`as.igraph()`](https://r.igraph.org/reference/as.igraph.html), applies
`.f`, and returns the result unchanged. This is the read-only lane onto
the whole igraph ecosystem: measures such as
[`degree()`](https://r.igraph.org/reference/degree.html),
[`betweenness()`](https://r.igraph.org/reference/betweenness.html),
[`coreness()`](https://r.igraph.org/reference/coreness.html), community
detection, and distances all work through this one entry point.

It is deliberately *not* a graph-modification path. If `.f` returns a
graph (e.g.
[`simplify()`](https://r.igraph.org/reference/simplify.html),
[`induced_subgraph()`](https://r.igraph.org/reference/subgraph.html)),
`gs_compute()` errors, because reintegrating a modified graph must go
through the graph-modification verb so that node, edge, and coordinate
data stay consistent.

## Usage

``` r
gs_compute(gs, .f, ...)
```

## Arguments

- gs:

  A `GraphSpace` object.

- .f:

  An igraph function, or the name of one as a string.

- ...:

  Further arguments passed on to `.f`.

## Value

Whatever `.f` returns (typically a named vector, matrix, or summary),
aligned to the graph's vertex order.

## Examples

``` r
if (FALSE) { # \dontrun{
gs_compute(gs, degree)
gs_compute(gs, "betweenness", directed = FALSE)
gs_compute(gs, cluster_louvain)

## fold a per-vertex result back as a node attribute:
gs$degree <- gs_compute(gs, degree)
} # }
```
