# Add nodes to a GraphSpace object

`gs_add_nodes()` and `gs_add_nodes<-` add one or more nodes to a
[`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object. The `@graph`, `@nodes`, and `@fdata` slots are updated
consistently. Because new nodes introduce coordinates into the existing
layout, the normalized state is invalidated and
[`normalizeGraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/normalizeGraphSpace-methods.md)
must be re-run afterwards.

`gs_add_nodes(x, value)` is the pipe-friendly functional form and
returns the modified object. `gs_add_nodes(x) <- value` is the in-place
replacement form and modifies `x` by reference in the calling
environment. Both forms are equivalent.

## Usage

``` r
# S4 method for class 'GraphSpace'
gs_add_nodes(x, value, ...)

# S4 method for class 'GraphSpace'
gs_add_nodes(x) <- value
```

## Arguments

- x:

  A
  [`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
  object.

- value:

  A data frame with at minimum three columns:

  - `name` — unique node identifier (character).

  - `x`, `y` — node coordinates in raw graph space.

  Any additional columns are treated as node attributes. Standard visual
  attributes (`nodeSize`, `nodeColor`, `nodeShape`, etc.) are filled
  from package defaults when omitted. The column `vertex` is reserved
  and stripped automatically if present.

- ...:

  Additional arguments (currently unused; reserved for future use).

## Value

A
[`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object with the new nodes appended and the normalized state cleared.

## Details

Adding nodes always invalidates the normalized layout. The `@pars`
normalization flags are cleared, `@canvas` is reset, and
`normalizeGraphSpace` must be re-run to restore a renderable state. The
`@edges` slot is not affected. Existing node coordinates in `@nodes`
revert to raw graph-space values if the object was previously
normalized, since normalization is cleared before `@nodes` is rebuilt.

Standard node attributes (`nodeSize`, `nodeColor`, etc.) are kept
consistent across old and new nodes: attributes present on existing
nodes but absent from `value` are filled from package defaults for the
new rows, and vice versa.

`nodeLabel` defaults to the node `name` when not supplied, consistent
with the behaviour of the
[`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
constructor.

If `@fdata` is non-empty, new nodes are appended as `NA` rows so the
feature matrix remains aligned with `@nodes`.

## See also

[`gs_add_edges`](https://sysbiolab.github.io/RGraphSpace/reference/gs_add_edges.md),
[`gs_vertex_attr`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-accessors.md),
[`gs_subset_nodes`](https://sysbiolab.github.io/RGraphSpace/reference/gs_subset.md),
[`gs_nodes`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-accessors.md)

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

# Functional form (pipe-friendly): returns a modified copy
gs <- gs_add_nodes(gs, data.frame(name = "n6", x = 0.5, y = 0.5))

# Assignment form: modifies gs in place
gs_add_nodes(gs) <- data.frame(name = "n7", x = 0.5, y = 0.5)

# Add multiple nodes with visual attributes
gs <- gs_add_nodes(gs, data.frame(
  name      = c("n8", "n9"),
  x         = c(0.5, 0.8),
  y         = c(0.5, 0.2),
  nodeSize  = c(8, 5),
  nodeColor = c("steelblue", "tomato")
))
```
