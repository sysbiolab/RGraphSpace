# Subscript operators for GraphSpace objects

`[` subsets a
[`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object along two independent dimensions: the first index (`i`) selects
nodes; the second (`j`) selects edges.

`[[` retrieves a single named slot from a
[`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object.

## Usage

``` r
# S4 method for class 'GraphSpace,ANY,ANY,ANY'
x[i, j, ..., drop = TRUE]

# S4 method for class 'GraphSpace'
x[[i, j, ...]]
```

## Arguments

- x:

  A
  [`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
  object.

- i:

  A node selection. Accepted forms:

  - A **character** vector of node names.

  - An **integer** vector of positional indices into `@nodes`.

  - A **logical** vector whose length matches the number of nodes.

  If omitted, all nodes are retained.

- j:

  An edge selection. Accepted forms:

  - An **integer** vector of positional indices into `@edges`.

  - A **logical** vector whose length matches the number of edges.

  Because `[` evaluates its arguments in the calling environment before
  dispatch, unquoted column names such as `name1 == "n1"` cannot be used
  directly. Pre-evaluate the expression against the edge table first
  (e.g. `gs_edges(gs)$name1 == "n1"`), or use
  [`gs_subset_edges`](https://sysbiolab.github.io/RGraphSpace/reference/gs_subset.md)
  which supports unquoted predicates via data masking. If omitted, all
  edges are retained (subject to node-filter cascade).

- ...:

  Currently unused.

- drop:

  Ignored; accepted for S4 method compatibility only.

## Value

`[` returns a
[`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object.

`[[` returns the content of the named slot.

## Details

**Mental model:** unlike a data frame, where `[i, j]` means rows and
columns of the same table, for `GraphSpace` the two indices address the
two primary components of the graph: nodes (`i`) and edges (`j`).
Neither index subsets columns — they select graph entities.

**Synchronization rules:**

- `x[i, ]` — node-induced subgraph. After selecting nodes, edges are
  automatically pruned to those whose both endpoints survived.
  Normalized coordinates are preserved.

- `x[, j]` — edge selection. The node set is not modified; no node
  pruning occurs.

- `x[i, j]` — combined selection. Node filtering is applied first. Edge
  index `j` is resolved against the **original** edge table; an edge
  survives only if it appears in `j` *and* both its endpoints survived
  node filtering (silent intersection).

**Note for `[[`:** the slot accessor is read-only. Use the dedicated
replacement methods (`gs_image<-`, `gs_fdata<-`, `gs_vertex_attr<-`,
etc.) to modify slot contents.

## See also

[`gs_subset_nodes`](https://sysbiolab.github.io/RGraphSpace/reference/gs_subset.md),
[`gs_subset_edges`](https://sysbiolab.github.io/RGraphSpace/reference/gs_subset.md),
[`getGraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/getGraphSpace-methods.md),
[`cropGraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/normalizeGraphSpace-methods.md)

## Examples

``` r
library(RGraphSpace)
library(igraph)

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

#--- [ examples ---

# Node-induced subgraph: keep named nodes, prune dangling edges
gs[c("n1", "n2", "n3"), ]
#> A GraphSpace-class object for:
#> IGRAPH a46a65d DNW- 3 2 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | arrowType (e/n), weight (e/n)
#> + node spatial boundaries: normalized to graph space
#> | x: [-1, 10] -> [0, 1] (cols)
#> | y: [-1, 4] -> [0, 1] (rows)

# Node-induced subgraph by integer position
gs[1:4, ]
#> A GraphSpace-class object for:
#> IGRAPH d3fe155 DNW- 4 3 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | arrowType (e/n), weight (e/n)
#> + node spatial boundaries: normalized to graph space
#> | x: [-1, 10] -> [0, 1] (cols)
#> | y: [-1, 4] -> [0, 1] (rows)

# Node-induced subgraph by pre-evaluated logical mask
gs[gs$nodeSize > 5, ]
#> A GraphSpace-class object for:
#> IGRAPH 52492b4 DNW- 6 0 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | arrowType (e/n), weight (e/n)
#> + node spatial boundaries: normalized to graph space
#> | x: [-5, 10] -> [0, 1] (cols)
#> | y: [-4, 3] -> [0, 1] (rows)

# Edge selection only: keep all nodes
gs[, 1:3]
#> A GraphSpace-class object for:
#> IGRAPH b2b3cfa DNW- 10 3 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | arrowType (e/n), weight (e/n)
#> + node spatial boundaries: normalized to graph space
#> | x: [-5, 10] -> [0, 1] (cols)
#> | y: [-4, 4] -> [0, 1] (rows)
gs[, gs_edges(gs)$weight > 0.5]
#> A GraphSpace-class object for:
#> IGRAPH 36d440a DNW- 10 3 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | arrowType (e/n), weight (e/n)
#> + node spatial boundaries: normalized to graph space
#> | x: [-5, 10] -> [0, 1] (cols)
#> | y: [-4, 4] -> [0, 1] (rows)

# Edge selection by endpoint: 'name1' and 'name2' must be pre-evaluated
# when using [, because [ evaluates j in the calling environment.
# Use gs_subset_edges() for unquoted predicate expressions instead.
gs[, gs_edges(gs)$name1 == "n1"]
#> A GraphSpace-class object for:
#> IGRAPH d6edeb0 DNW- 10 9 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | arrowType (e/n), weight (e/n)
#> + node spatial boundaries: normalized to graph space
#> | x: [-5, 10] -> [0, 1] (cols)
#> | y: [-4, 4] -> [0, 1] (rows)
gs[, gs_edges(gs)$name1 == "n1" & gs_edges(gs)$name2 == "n2"]
#> A GraphSpace-class object for:
#> IGRAPH 0bbb6db DNW- 10 1 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | arrowType (e/n), weight (e/n)
#> + node spatial boundaries: normalized to graph space
#> | x: [-5, 10] -> [0, 1] (cols)
#> | y: [-4, 4] -> [0, 1] (rows)
gs[, quote(name1 == "n1" & name2 == "n2")]
#> A GraphSpace-class object for:
#> IGRAPH 80b6ba2 DNW- 10 1 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | arrowType (e/n), weight (e/n)
#> + node spatial boundaries: normalized to graph space
#> | x: [-5, 10] -> [0, 1] (cols)
#> | y: [-4, 4] -> [0, 1] (rows)

# Combined: node filter first, then edge intersection
gs[c("n1", "n2", "n3"), gs_edges(gs)$weight > 0.5]
#> Warning: No edges matched the filter expression.
#> ℹ The returned object contains no edges.
#> A GraphSpace-class object for:
#> IGRAPH b9ed9db DNW- 3 0 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | arrowType (e/n), weight (e/n)
#> + node spatial boundaries: normalized to graph space
#> | x: [-1, 10] -> [0, 1] (cols)
#> | y: [-1, 4] -> [0, 1] (rows)
gs[c("n1", "n2", "n3"), gs_edges(gs)$name1 == "n1"]
#> A GraphSpace-class object for:
#> IGRAPH 9ddc9be DNW- 3 2 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | arrowType (e/n), weight (e/n)
#> + node spatial boundaries: normalized to graph space
#> | x: [-1, 10] -> [0, 1] (cols)
#> | y: [-1, 4] -> [0, 1] (rows)

#--- [[ examples ---

gs[["nodes"]]   # same as getGraphSpace(gs, "nodes")
#>     vertex         x         y name nodeLabel nodeSize
#> n1       1 0.2968267 0.5002836   n1        n1 2.922736
#> n2       2 0.9000000 0.5315351   n2        n2 5.344287
#> n3       3 0.3723027 0.7015267   n3        n3 3.084499
#> n4       4 0.3618994 0.4578191   n4        n4 5.382545
#> n5       5 0.1142670 0.6741392   n5        n5 8.057340
#> n6       6 0.4035739 0.2984733   n6        n6 7.717220
#> n7       7 0.3636256 0.5578518   n7        n7 5.544150
#> n8       8 0.1000000 0.4185991   n8        n8 9.359878
#> n9       9 0.2512082 0.3966998   n9        n9 4.194233
#> n10     10 0.2326128 0.5414368  n10       n10 1.487138
gs[["edges"]]   # same as getGraphSpace(gs, "edges")
#>   vertex1 vertex2 name1 name2 arrowType      weight curve_weight is_multiple
#> 1       1       2    n1    n2         1 0.006984655            1       FALSE
#> 2       1       3    n1    n3         1 0.112558847            1       FALSE
#> 3       1       4    n1    n4         1 0.905431530            1       FALSE
#> 4       1       5    n1    n5         1 0.077314988            1       FALSE
#> 5       1       6    n1    n6         1 0.098826466            1       FALSE
#> 6       1       7    n1    n7         1 0.686723741            1       FALSE
#> 7       1       8    n1    n8         1 0.118696214            1       FALSE
#> 8       1       9    n1    n9         1 0.412838486            1       FALSE
#> 9       1      10    n1   n10         1 0.859673030            1       FALSE
#>   is_loop
#> 1   FALSE
#> 2   FALSE
#> 3   FALSE
#> 4   FALSE
#> 5   FALSE
#> 6   FALSE
#> 7   FALSE
#> 8   FALSE
#> 9   FALSE
gs[["graph"]]   # same as getGraphSpace(gs, "graph")
#> IGRAPH d6edeb0 DNW- 10 9 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | arrowType (e/n), weight (e/n)
#> + edges from d6edeb0 (vertex names):
#> [1] n1->n2  n1->n3  n1->n4  n1->n5  n1->n6  n1->n7  n1->n8  n1->n9  n1->n10
gs[["fdata"]]   # same as getGraphSpace(gs, "fdata")
#> 0 x 0 diagonal matrix of class "ldiMatrix"
#> <0 x 0 matrix>
```
