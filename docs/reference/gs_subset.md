# Filter nodes and edges in a GraphSpace object

`gs_subset_nodes()` retains a subset of nodes and automatically removes
any edge whose endpoint is no longer present.

`gs_subset_edges()` retains a subset of edges without modifying the node
set.

## Usage

``` r
gs_subset_nodes(x, i)

gs_subset_edges(x, i)
```

## Arguments

- x:

  A
  [`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
  object.

- i:

  A filter specification. Accepted forms:

  - A **character** vector of node names (`gs_subset_nodes()` only;
    edges are identified by integer position or predicate, not by name).

  - An **integer** vector of positional indices into the node or edge
    table.

  - A **logical** vector whose length must match the number of nodes or
    edges, respectively.

  - An **unquoted predicate** evaluated against the node or edge data
    frame using data masking, such as `nodeSize > 5` or `weight > 0.5`.
    Column names from the relevant table are available directly as
    variables inside the expression.

## Value

A
[`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object with the selected subset of nodes or edges.

## Details

**Node filtering** preserves the normalized coordinate state.
Coordinates for surviving nodes remain in their current space (`[0, 1]`
if normalized, raw coordinates otherwise), so
[`normalizeGraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/normalizeGraphSpace-methods.md)
does not need to be re-run. The `@graph`, `@fdata`, `@nodes`, and
`@edges` slots are all updated consistently. The `@canvas` and
background image are not modified.

**Edge filtering** leaves the node set and the layout entirely intact.
Because removing an edge from a group of parallel edges invalidates the
derived attributes `curve_weight`, `is_multiple`, and `is_loop` for the
remaining members of that group, the full edge table is recomputed from
`@graph` after deletion.

**Note on parallel edges**: in non-simplified graphs containing parallel
edges between the same vertex pair, integer or logical indexing is the
most reliable approach. A predicate expression that matches a shared
attribute (such as `edgeColor`) will match all parallel instances
simultaneously, which is usually the intended behavior.

## See also

[`cropGraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/cropGraphSpace-methods.md),
[`gs_nodes`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-accessors.md),
[`gs_edges`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-accessors.md),
[`normalizeGraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/normalizeGraphSpace-methods.md)

## Examples

``` r
library(RGraphSpace)
library(igraph)

# Create a directed star graph with numeric attributes
g <- make_star(10, mode = "out")
V(g)$nodeSize <- runif(vcount(g), 1, 10)
E(g)$weight   <- runif(ecount(g), 0, 1)
gs <- GraphSpace(g)
#> Validating the 'igraph' object...
#> Vertex attributes 'x' and 'y' missing; computing layout...
#> Vertex attribute 'name' missing; assigning names... 
#> Ignoring graph-level attributes: 'name', 'mode', 'center'
#> Creating a 'GraphSpace' object...
gs <- normalizeGraphSpace(gs)
#> Normalizing node coordinates to graph space...

#--- gs_subset_nodes examples ---

# By node name (character vector)
gs2 <- gs_subset_nodes(gs, c("n1", "n2", "n3"))

# By integer position
gs2 <- gs_subset_nodes(gs, 1:5)

# By predicate (data masking against @nodes columns)
gs2 <- gs_subset_nodes(gs, nodeSize > 5)

# By pre-evaluated logical vector
keep <- gs$nodeSize > 5
gs2  <- gs_subset_nodes(gs, keep)

# Combining with pipes
gs2 <- gs |>
  gs_subset_nodes(nodeSize > 5) |>
  gs_subset_edges(weight > 0.3)
#> Warning: The 'GraphSpace' object has no edges to filter.

#--- gs_subset_edges examples ---

# By predicate on an edge attribute
gs3 <- gs_subset_edges(gs, weight > 0.5)

# By endpoint names: name1 and name2 are columns in @edges and
# can be used directly inside any predicate expression
gs3 <- gs_subset_edges(gs, name1 == "n1")
gs3 <- gs_subset_edges(gs, name2 == "n1")
#> Warning: No edges matched the filter expression.
#> ℹ The returned object contains no edges.

# Combining endpoint and attribute conditions
gs3 <- gs_subset_edges(gs, name1 == "n1" & weight > 0.5)

# By integer position
gs3 <- gs_subset_edges(gs, 1:3)

# By logical vector
gs3 <- gs_subset_edges(gs, gs_edges(gs)$weight > 0.5)
```
