# Add edges to a GraphSpace object

`gs_add_edges()` and `gs_add_edges<-` add one or more edges to a
[`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object. Both endpoints of every new edge must already exist in the node
set. The `@graph`, `@edges`, and all derived edge quantities are updated
consistently; the node set and the normalized coordinate state are not
affected.

`gs_add_edges(x, value)` is the pipe-friendly functional form and
returns the modified object. `gs_add_edges(x) <- value` is the in-place
replacement form and modifies `x` by reference in the calling
environment. Both forms are equivalent.

## Usage

``` r
# S4 method for class 'GraphSpace'
gs_add_edges(x, value, ...)

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

- ...:

  Additional arguments (currently unused; reserved for future use).

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

[`gs_add_nodes`](https://sysbiolab.github.io/RGraphSpace/reference/gs_add_nodes.md),
[`gs_edge_attr`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-accessors.md),
[`gs_subset_edges`](https://sysbiolab.github.io/RGraphSpace/reference/gs_subset.md),
[`gs_edges`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-accessors.md)

## Examples

``` r
library(RGraphSpace)
library(igraph)

g <- make_star(6, mode = "out")
gs <- GraphSpace(g)
#> Validating the 'igraph' object...
#> Vertex attributes 'x' and 'y' missing; computing layout...
#> Vertex attribute 'name' missing; assigning names... 
#> Ignoring graph-level attributes: 'name', 'mode', 'center'
#> Creating a 'GraphSpace' object...
gs <- normalizeGraphSpace(gs)
#> Normalizing node coordinates to graph space...

# Functional form (pipe-friendly): returns a modified copy
gs <- gs_add_edges(gs, data.frame(from = "n2", to = "n3"))

# Assignment form: modifies gs in place
gs_add_edges(gs) <- data.frame(from = "n3", to = "n4")

# Add multiple edges with an analytical attribute
gs <- gs_add_edges(gs, data.frame(
  from   = c("n4", "n5"),
  to     = c("n5", "n6"),
  weight = c(0.8, 0.4)
))
```
