# Apply igraph functions to the graph inside a GraphSpace

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
this cannot be reintegrated as a modified graph must go through the
graph-modification checks.

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
library(RGraphSpace)
library(igraph)

# Load a demo igraph
data('gtoy1', package = 'RGraphSpace')

# Create a new GraphSpace object
gs <- GraphSpace(gtoy1)
#> Validating the 'igraph' object...
#> Ignoring graph-level attributes: 'name', 'mode', 'center'
#> Creating a 'GraphSpace' object...

# Apply igraph functions
gs_compute(gs, igraph::degree)
#> n1 n2 n3 n4 n5 
#>  4  1  1  1  1 
gs_compute(gs, "betweenness", directed = FALSE)
#> n1 n2 n3 n4 n5 
#>  6  0  0  0  0 

# Fold a per-vertex result back as a node attribute
gs$degree <- gs_compute(gs, igraph::degree)
```
