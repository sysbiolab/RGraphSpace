# Add edges to a GraphSpace object

`gs_add_edges<-` adds one or more edges to a
[`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object. Both endpoints of every new edge must already exist in the node
set. The `@graph`, `@edges`, and all derived edge quantities are updated
consistently; the node set and the normalized coordinate state are not
affected.

## Usage

``` r
# S4 method for class 'GraphSpace'
gs_add_edges(x) <- value
```

## Arguments

- x:

  A
  [`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
  object.

- value:

  A data frame with at least two columns identifying the edge endpoints.
  Two column naming conventions are accepted:

  - `from` / `to` — the tidygraph / igraph convention.

  - `name1` / `name2` — the `@edges` slot convention, useful when
    constructing `value` directly from
    [`gs_edges()`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-accessors.md).

  If both conventions are present, `from`/`to` takes priority. Any
  additional columns are treated as edge attributes and passed through
  to `@edges`. Standard visual attributes (`edgeColor`, `arrowType`,
  etc.) are filled from package defaults when omitted; analytical
  attributes such as `weight` are stored as-is.

## Value

A
[`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object with the new edges appended.

## Details

Adding edges does not invalidate the normalized layout. Node coordinates
in `@nodes` are left untouched and `normalizeGraphSpace` does not need
to be re-run.

For objects built with `simplify = TRUE` (the default), loop edges
(`from == to`), parallel edges, and duplicate rows within `value` are
silently dropped with a warning. Admissible edges in the same call are
still added. To allow loops or parallel edges, rebuild the object with
`GraphSpace(g, simplify = FALSE)`.

Because adding an edge to a group of parallel edges changes the derived
attributes `curve_weight`, `is_multiple`, and `is_loop` for all members
of that group, the full edge table is recomputed from `@graph` after
each assignment.

## See also

`gs_add_nodes<-`, `gs_edge_attr<-`,
[`gs_subset_edges`](https://sysbiolab.github.io/RGraphSpace/reference/gs_subset.md),
[`gs_edges`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-accessors.md)

## Examples

``` r
library(RGraphSpace)
library(igraph)

g <- make_star(5, mode = "out")
gs <- GraphSpace(g)
#> Validating the 'igraph' object...
#> Vertex attributes 'x' and 'y' missing; computing layout...
#> Vertex attribute 'name' missing; assigning names... 
#> Ignoring graph-level attributes: 'name', 'mode', 'center'
#> Creating a 'GraphSpace' object...
gs <- normalizeGraphSpace(gs)
#> Normalizing node coordinates to graph space...

# Add a single edge using from/to convention
gs_add_edges(gs) <- data.frame(from = "n2", to = "n3")

# Add multiple edges with an analytical attribute
gs_add_edges(gs) <- data.frame(
  from   = c("n2", "n3"),
  to     = c("n4", "n5"),
  weight = c(0.8, 0.4)
)

# name1/name2 convention also accepted (e.g. from gs_edges() output)
gs_add_edges(gs) <- data.frame(name1 = "n2", name2 = "n3")
#> Warning: ! 1 parallel edge(s) ignored: simplified GraphSpace do not allow parallel edges.
#> ℹ n2 -> n3
#> • Rebuild with GraphSpace(g, simplify = FALSE) to allow parallel edges.
```
