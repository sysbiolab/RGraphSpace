# GraphSpace accessors and transformations

\
**Package**: RGraphSpace 1.5.5

``` r

# Check required version
if (packageVersion("RGraphSpace") < "1.5.5"){
  message("Need to update 'RGraphSpace' for this vignette")
  remotes::install_github("sysbiolab/RGraphSpace")
}
```

## Overview

*RGraphSpace* is primarily designed for graph rendering and expects
graphs in their final form before they enter its workflow. More
extensive graph transformations are therefore best performed upstream,
while preparing the input data.

Nevertheless, downstream analyses may require accessing and modifying a
`GraphSpace` object to explore alternative graph configurations or
highlight specific subsets of the data.

In the following, we demonstrate the `GraphSpace` accessors for graph
manipulation. All general accessors use the `gs_*` prefix to reduce the
chance of masking functions from other packages, particularly
graph-analysis packages that are commonly used side-by-side.

### Setting basic input data

``` r

#--- Load required packages
library("RGraphSpace")
library("igraph")
library("ggplot2")
```

``` r

# Make a toy modular graph
set.seed(42)
g <- sample_islands(
  islands.n = 3,       # number of modules
  islands.size = 30,   # nodes per module
  islands.pin = 0.25,  # probability of edges within modules
  n.inter = 2)         # edges between modules

# Assign module membership to nodes
V(g)$module <- rep(1:3, each = 30)

# Assign colors to nodes
V(g)$nodeFillColor <- rainbow(3)[V(g)$module]

# Assign a categorical variable to nodes
V(g)$node_group <- c("A", "B", "C")[V(g)$module]

# Assign numeric variables to nodes and edges
V(g)$node_var <- rnorm(vcount(g))
E(g)$edge_var <- rnorm(ecount(g))

# Create a GraphSpace object
gs <- GraphSpace(g, simplify = FALSE)
```

## Manipulating attributes

The
[`gs_vertex_attr()`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-attributes.md)
and
[`gs_edge_attr()`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-attributes.md)
functions provide a single interface to access, add, modify, and delete
node and edges attributes, respectively. This interface wraps the
corresponding *igraph* methods, applying them to the internal graph
representation and propagating the result to all `GraphSpace`
components.

### Node attributes

``` r

# Access all vertex attributes
# gs_vertex_attr(gs)

# Access a specific vertex attribute
gs_vertex_attr(gs, "node_group")[1:5]
#>  n1  n2  n3  n4  n5 
#> "A" "A" "A" "A" "A"

# Add or replace an entire vertex attribute
gs_vertex_attr(gs, "nodeSize") <- 5

# Modify a single value within a vertex attribute
gs_vertex_attr(gs, "nodeSize")["n1"] <- 10

# Add a new vertex variable
gs_vertex_attr(gs, "new_node_var") <- rnorm(gs_vcount(gs))

# Delete a vertex attribute or variable by assigning NULL
gs_vertex_attr(gs, "new_node_var") <- NULL

gs
#> A GraphSpace-class object for:
#> IGRAPH d3c377c UN-- 90 329 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | nodeFillColor (v/c), module (v/n), node_group (v/c), node_var (v/n),
#> | arrowType (e/n), edge_var (e/n)
#> + node spatial boundaries: raw graph
#> | x: [-8, 11] (cols)
#> | y: [-10, 8] (rows)
```

### Edge attributes

``` r

# Access a specific edge attribute
gs_edge_attr(gs, "arrowType")[1:3]
#> [1] 0 0 0

# Add or replace an entire edge attribute
gs_edge_attr(gs, "edgeColor") <- "grey"

# Add a new edge variable
gs_edge_attr(gs, "new_edge_var") <- rnorm(gs_ecount(gs))

# Delete an edge attribute or variable by assigning NULL
gs_edge_attr(gs, "new_edge_var") <- NULL

gs
#> A GraphSpace-class object for:
#> IGRAPH d3c377c UN-- 90 329 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | nodeFillColor (v/c), module (v/n), node_group (v/c), node_var (v/n),
#> | edgeColor (e/c), arrowType (e/n), edge_var (e/n)
#> + node spatial boundaries: raw graph
#> | x: [-8, 11] (cols)
#> | y: [-10, 8] (rows)
```

### Protected attributes

Some node and edge attributes are protected from certain edits. The
`name` and `vertex` attributes are read-only: the package relies on them
internally, so they cannot be modified, and attempting to assign to one
raises an error. A second group, `x`, `y`, `nodeLabel`, and `nodeSize`,
can be modified but not deleted, since they are required for essential
operations. Reading any of these is unaffected. For edges, `arrowType`
is the only protected attribute exposed to the user; it can be modified
but not deleted. The remaining protected edge attributes, `vertex1`,
`vertex2`, `name1`, and `name2`, are read-only and hidden from the
interface, since they are derived from the node data.

``` r

# Return the vertex attribute `name`
gs_vertex_attr(gs, "name")[1:5]
#> [1] "n1" "n2" "n3" "n4" "n5"

# Protected attribute that cannot be modified
# gs_vertex_attr(gs, "name") <- 1  
#> error: 'name' is a read-only node attribute.

# Protected attribute that cannot be deleted
# gs_vertex_attr(gs, "x") <- NULL 
#> error: 'x' is a protected node attribute.
```

See the [Building a
GraphSpace](https://sysbiolab.github.io/RGraphSpace/articles/building-graphspace.html#rgraphspace-attributes)
tutorial for a detailed list of *RGraphSpace* attributes.

## Adding nodes

The
[`gs_add_nodes()`](https://sysbiolab.github.io/RGraphSpace/reference/gs_add_nodes.md)
function adds one or more nodes to a `GraphSpace` object. Attributes
present on existing nodes but absent in the input `value` are filled
from package defaults. Standard node attributes (such as `nodeSize` and
`nodeColor`) are kept consistent across old and new nodes; the `@graph`,
`@nodes`, and `@fdata` slots are updated consistently. Because new nodes
introduce coordinates into the existing layout, the normalized state is
invalidated and
[`normalizeGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/normalizeGraphSpace-methods.md)
must be re-run afterwards.

``` r

# Functional form (pipe-friendly) returns a modified copy
gs <- gs |> gs_add_nodes(data.frame(name = "new1", x = 0, y = -2))

# Assignment form modifies gs in place
gs_add_nodes(gs) <- data.frame(name = "new2", x = 0, y = 2)

# Add multiple nodes with visual attributes
gs <- gs_add_nodes(gs, data.frame(
  name = c("new3", "new4"),
  x = c(-2, 2),
  y = c(0, 0),
  nodeSize = c(8, 5),
  nodeFillColor = c("steelblue", "tomato")) )

# Add two nodes; x and y are assigned random values
gs_add_nodes(gs) <- c("new5", "new6")
```

## Adding edges

The
[`gs_add_edges()`](https://sysbiolab.github.io/RGraphSpace/reference/gs_add_edges.md)
function adds one or more edges to a `GraphSpace` object. Both endpoints
of every new edge must already exist in the node set. The `@graph` and
`@edges` slots, along with all derived edge quantities, are updated
consistently; the node set and the normalized coordinate state are not
affected.

``` r

# Functional form (pipe-friendly) returns a modified copy
gs <- gs |> gs_add_edges(data.frame(from = "new1", to = "new2"))

# Assignment form modifies gs in place
gs_add_edges(gs) <- data.frame(from = "new3", to = "new4")

# Add multiple edges with a numeric attribute
gs <- gs_add_edges(gs, data.frame(
  from = c("new1", "new3"),
  to = c("new2", "new4"),
  edge_var = c(10, 20)) )
```

For objects built with `simplify = TRUE` (the default), loop edges
(`from == to`), parallel edges, and duplicate rows within `value` are
dropped with a warning. To allow loops or parallel edges, rebuild the
object with `GraphSpace(g, simplify = FALSE)`.

## Subsetting nodes

The
[`gs_subset_nodes()`](https://sysbiolab.github.io/RGraphSpace/reference/gs_subset.md)
function retains a subset of nodes and automatically removes any edge
whose endpoint is no longer present. The result is propagated to all
`GraphSpace` components.

``` r

# Subset by node name (character vector)
gs2 <- gs_subset_nodes(gs, c("n1", "n2", "n3"))

# Subset by integer position
gs2 <- gs_subset_nodes(gs, 1:5)

# Subset by predicate (data masking against @nodes columns)
gs2 <- gs_subset_nodes(gs, node_group != "A")

# Subset by pre-evaluated logical vector
keep <- gs$node_var > 0
gs2  <- gs_subset_nodes(gs, keep)

# Combining with pipes
gs2 <- gs |>
  gs_subset_nodes(node_group == "C") |>
  gs_subset_edges(edge_var > 0)
```

## Subsetting edges

The
[`gs_subset_edges()`](https://sysbiolab.github.io/RGraphSpace/reference/gs_subset.md)
retains a subset of edges without modifying the node set, and the result
is propagated to all `GraphSpace` components.

``` r

# Subset by predicate on an edge attribute
gs2 <- gs_subset_edges(gs, edge_var > 0)

# Subset by endpoint names: 'name1' and 'name2' are columns in
# @edges and can be used directly inside any predicate expression
gs2 <- gs_subset_edges(gs, name1 == "new1" & name2 == "new2")

# Combining endpoint and attribute conditions
gs2 <- gs_subset_edges(gs, name1 == "new1" & edge_var > 0)

# By integer position
gs2 <- gs_subset_edges(gs, 1:3)

# By logical vector
gs2 <- gs_subset_edges(gs, gs_edges(gs)$edge_var > 0)
```

## Subscript operators

The `[` operator subsets a `GraphSpace` object along two independent
dimensions: nodes (`i`) and edges (`j`). This differs from the usual
data-frame convention, where `[i, j]` refers to rows and columns of a
single table. Here, neither index subsets columns; both select graph
entities directly. Omitting an index retains all elements along that
dimension.

**Synchronization rules:**

- `x[i, ]` **Node-induced subgraph.** Nodes are selected by `i`, then
  edges are automatically pruned to those whose endpoints both survived.
  Normalized coordinates are preserved.
- `x[, j]` **Edge selection.** Edges are selected by `j`; the node set
  is untouched and no node pruning occurs.
- `x[i, j]` **Combined selection.** Node filtering is applied first,
  then `j` is evaluated against the **original**, unfiltered edge table;
  an edge survives only if it satisfies `j` **and** both its endpoints
  survived node filtering.

``` r

# Node-induced subgraph: keep named nodes, prune dangling edges
gs[c("n1", "n2", "n3"), ]
#> A GraphSpace-class object for:
#> IGRAPH 278489e UN-- 3 1 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | nodeFillColor (v/c), module (v/n), node_group (v/c), node_var (v/n),
#> | edgeColor (e/c), arrowType (e/n), edge_var (e/n)
#> + node spatial boundaries: raw graph
#> | x: [4, 7] (cols)
#> | y: [3, 4] (rows)

# Node-induced subgraph by integer position
gs[1:4, ]
#> A GraphSpace-class object for:
#> IGRAPH 58c5d8a UN-- 4 3 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | nodeFillColor (v/c), module (v/n), node_group (v/c), node_var (v/n),
#> | edgeColor (e/c), arrowType (e/n), edge_var (e/n)
#> + node spatial boundaries: raw graph
#> | x: [4, 7] (cols)
#> | y: [2, 4] (rows)

# Node-induced subgraph by pre-evaluated logical mask
gs[gs$node_var > 0, ]
#> A GraphSpace-class object for:
#> IGRAPH a195fcc UN-- 39 70 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | nodeFillColor (v/c), module (v/n), node_group (v/c), node_var (v/n),
#> | edgeColor (e/c), arrowType (e/n), edge_var (e/n)
#> + node spatial boundaries: raw graph
#> | x: [-8, 8] (cols)
#> | y: [-9, 7] (rows)

# Edge selection only: keep all nodes
gs[, gs_edges(gs)$edge_var > 0]
#> A GraphSpace-class object for:
#> IGRAPH 1bb81e7 UN-- 96 161 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | nodeFillColor (v/c), module (v/n), node_group (v/c), node_var (v/n),
#> | edgeColor (e/c), arrowType (e/n), edge_var (e/n)
#> + node spatial boundaries: raw graph
#> | x: [-8, 11] (cols)
#> | y: [-10, 8] (rows)

# Edge selection by endpoint: predicates must be pre-evaluated
gs[, gs_edges(gs)$name1 == "n1"]
#> A GraphSpace-class object for:
#> IGRAPH 3bac86b UN-- 96 10 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | nodeFillColor (v/c), module (v/n), node_group (v/c), node_var (v/n),
#> | edgeColor (e/c), arrowType (e/n), edge_var (e/n)
#> + node spatial boundaries: raw graph
#> | x: [-8, 11] (cols)
#> | y: [-10, 8] (rows)

# Alternatively, wrap the predicate in quote()
gs[, quote(name1 == "n1" & edge_var > 0)]
#> A GraphSpace-class object for:
#> IGRAPH e7cc12c UN-- 96 3 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | nodeFillColor (v/c), module (v/n), node_group (v/c), node_var (v/n),
#> | edgeColor (e/c), arrowType (e/n), edge_var (e/n)
#> + node spatial boundaries: raw graph
#> | x: [-8, 11] (cols)
#> | y: [-10, 8] (rows)

# Combined: node filter first, then edge intersection
gs[quote(node_group == "A"), gs_edges(gs)$edge_var > 0]
#> A GraphSpace-class object for:
#> IGRAPH 24234c8 UN-- 30 49 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | nodeFillColor (v/c), module (v/n), node_group (v/c), node_var (v/n),
#> | edgeColor (e/c), arrowType (e/n), edge_var (e/n)
#> + node spatial boundaries: raw graph
#> | x: [3, 11] (cols)
#> | y: [-2, 5] (rows)
```

The `[[` operator, by contrast, is a simple accessor: `x[["nodes"]]`,
`x[["edges"]]`, `x[["graph"]]`, and `x[["fdata"]]` return the
corresponding component unmodified, equivalent to
`getGraphSpace(x, "nodes")` and so on. No subsetting or synchronization
logic applies.

``` r

gs[["nodes"]]   # same as getGraphSpace(gs, "nodes")
gs[["edges"]]   # same as getGraphSpace(gs, "edges")
gs[["graph"]]   # same as getGraphSpace(gs, "graph")
gs[["fdata"]]   # same as getGraphSpace(gs, "fdata")
```

## The `$` accessor

The `$` operator provides direct, attribute-style access to a single
**vertex attribute** of a `GraphSpace` object, similar to how `$` works
on a data frame or list. A companion `.DollarNames` method enables
tab-completion after `gs$` in interactive sessions (*e.g.* via RStudio
console), listing all columns currently in the node table.

Arbitrary attributes created via `$<-` are stored as **payload**: they
are added to the node table but not expanded into vertex attributes in
the underlying `igraph` object. [Geometries](#geometries) use this same
storage path to avoid expanding large, complex objects into vertex
attributes.

``` r

# Set an existing vertex attribute
gs$nodeSize <- 5

# Modify one element
gs$nodeSize[1] <- 10

# Add a new RGraphSpace vertex attribute
gs$nodeLineColor <- "grey"

# Add a new node variable (payload)
gs$new_node_var <- 1

gs
#> A GraphSpace-class object for:
#> IGRAPH c943123 UN-- 96 333 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | nodeFillColor (v/c), nodeLineColor (v/c), module (v/n), node_group
#> | (v/c), node_var (v/n), edgeColor (e/c), arrowType (e/n), edge_var
#> | (e/n)
#> + node payload: 1 (new_node_var)
#> + node spatial boundaries: raw graph
#> | x: [-8, 11] (cols)
#> | y: [-10, 8] (rows)
```

Assigning `NULL` deletes an attribute, whether stored as a vertex
attribute or as payload.

``` r

# Delete a vertex attribute
gs$nodeLineColor <- NULL

# Delete a payload
gs$new_node_var <- NULL

gs
#> A GraphSpace-class object for:
#> IGRAPH c943123 UN-- 96 333 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | nodeFillColor (v/c), module (v/n), node_group (v/c), node_var (v/n),
#> | edgeColor (e/c), arrowType (e/n), edge_var (e/n)
#> + node spatial boundaries: raw graph
#> | x: [-8, 11] (cols)
#> | y: [-10, 8] (rows)
```

## Other accessors

Here we show typical usage of other accessors used across the tutorials.
Output is omitted for brevity; run the calls interactively to see each
result.

### Graph-related accessors

``` r

# Vertex names
names(gs)

# Vertex attribute names
gs_names(gs)

# Return the node data frame
gs_nodes(gs)

# Return the edge data frame
gs_edges(gs)

# Return the underlying igraph object
gs_graph(gs)

# Number of vertices
gs_vcount(gs)

# Number of edges
gs_ecount(gs)
```

### Feature-related accessors

``` r

# Make a sparse Matrix aligned to nodes
mtx <- Matrix::Matrix(0, gs_vcount(gs), 2)
rownames(mtx) <- names(gs)
colnames(mtx) <- c("feature1", "feature2")

# Add a sparse matrix
gs_fdata(gs) <- mtx

# Feature names
gs_features(gs)

# Number of features
gs_nfeatures(gs)
```

### Image-related accessors

``` r

# Images may be provided as raster or numeric matrices;
# 'SpatRaster' objects are supported when the optional
# 'terra' package is available
gs_image(gs) <- as_colorraster(volcano)

# Set a pixel budget for image operations
gs_image_maxpixels(gs) <- 4e+06

# Scale node coordinates into image coordinate space
# (e.g. when node/image use different units/resolution)
gs_scale_factor(gs) <- 0.1

# Undo scaling
gs_scale_factor(gs) <- 1
```

## Spatial transformations

Spatial transformation functions are specialized accessors that operate
on a reference frame, either graph or image space, rather than on the
graph structure alone. They transform the spatial representation of a
`GraphSpace` object while preserving its underlying graph topology and
associated attributes. Combined, these transformations provide an
interface for accessing all eight symmetries of a square, including
support for cropping and resizing operations.

- [`normalizeGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/normalizeGraphSpace-methods.md)
  normalizes the graph and, when present, the background image into a
  common reference frame. The `image.space` argument controls which is
  used as the reference map.
- [`cropGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-transform.md)
  subsets a normalized `GraphSpace` to a specific region defined by
  cropping boundaries. It recalculates node positions and background
  image boundaries to maintain spatial consistency after cropping, and
  drops nodes (and edges) that fall outside the cropping window.
- [`rotateGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-transform.md),
  [`flipGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-transform.md),
  and
  [`transposeGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-transform.md)
  are all exact coordinate/pixel permutations, with no re-sampling or
  interpolation. Applied to a normalized `GraphSpace`, these operations
  transform the normalized coordinates, leaving the raw coordinates
  intact so the original orientation can be restored without loss.
  Applied to a `GraphSpace` that has not been normalized, they transform
  the raw coordinates permanently. The `persist` argument controls this
  behavior: `persist = TRUE` writes the transformation into the raw
  coordinates regardless of normalization state.

``` r

# Create a GraphSpace
gs <- GraphSpace(make_full_graph(30))

# Normalize coordinates
# The 'image.space' argument controls whether the image
# (when present) is used as the reference map
gs <- normalizeGraphSpace(gs, image.space = FALSE)

# Crop
gs_crop <- cropGraphSpace(gs, ymax = 0.5)

# Rotate
gs_rot90 <- rotateGraphSpace(gs)

# Flip
gs_flip <- flipGraphSpace(gs)

# Transpose
gs_t <- transposeGraphSpace(gs)

# Set 'persist' to TRUE to rotate, flip, or transpose
# the raw coordinates directly; the transformation
# will persist through re-normalization
gs_rot90 <- rotateGraphSpace(gs, persist = TRUE)
```

## Geometries

Geometry columns from the `sf` package (class `sfc`) are specialized
list-columns, where each element contains a spatial object, such as a
point, line, or polygon. *RGraphSpace* accepts these objects as node
payloads.

Unlike numeric, character, and logical attributes, geometry list-columns
add overhead that increases with graph size when stored as vertex
attributes. Node payload avoids this: geometries remain in the node
table and are not expanded into vertex attributes, so assigning or
updating them is a standard data-frame operation.

``` r

# Add an 'sfc' geometry column (requires the optional 'sf' package)
# 'sf' is optional to avoid introducing a heavy dependency
if (requireNamespace("sf", quietly = TRUE)) {
  gs_geometry(gs) <- sfshape_ngons(n = gs_vcount(gs))
}

gs
#> A GraphSpace-class object for:
#> IGRAPH c3caa65 UN-- 30 435 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | arrowType (e/n)
#> + node payload: 1 (geometry)
#> + node spatial boundaries: normalized to graph space
#> | x: [-1, 1] -> [0, 1] (cols)
#> | y: [-1, 1] -> [0, 1] (rows)
```

Other geometry accessors are cover in the [geometries and
nodes](https://sysbiolab.github.io/RGraphSpace/articles/geometries.md)
tutorial.

## Applying *igraph* functions

The
[`gs_compute()`](https://sysbiolab.github.io/RGraphSpace/reference/gs_compute.md)
function runs any igraph function on the graph carried by a
`GraphSpace`, without needing a dedicated `gs_*` wrapper for each one.
It extracts the underlying igraph via
[`as.igraph()`](https://r.igraph.org/reference/as.igraph.html), applies
`.f`, and returns the result unchanged. This is the read-only lane onto
the broader igraph ecosystem: measures such as
[`degree()`](https://r.igraph.org/reference/degree.html),
[`betweenness()`](https://r.igraph.org/reference/betweenness.html), and
[`coreness()`](https://r.igraph.org/reference/coreness.html), all work
through this single entry point.

``` r

# Apply igraph functions
gs_compute(gs, igraph::degree)[1:5]
#> n1 n2 n3 n4 n5 
#> 29 29 29 29 29

gs_compute(gs, "betweenness", directed = FALSE)[1:5]
#> n1 n2 n3 n4 n5 
#>  0  0  0  0  0

# Fold a per-vertex result back as a node attribute
gs$degree <- gs_compute(gs, igraph::degree)
```

It is deliberately *not* a GraphSpace-modification path. If `.f` returns
a graph (*e.g.*
[`simplify()`](https://r.igraph.org/reference/simplify.html),
[`induced_subgraph()`](https://r.igraph.org/reference/subgraph.html)),
this cannot be reintegrated to the `gs` object, as graph modifications
may invalidate the correspondence between the graph and other components
of the object. A modified graph must instead go through the
graph-modification checks of the
[`GraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
constructor.

## Session information

    #> R version 4.6.1 (2026-06-24)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Ubuntu 24.04.5 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    #> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    #>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    #>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    #>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    #>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    #> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    #> 
    #> time zone: America/Sao_Paulo
    #> tzcode source: system (glibc)
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] igraph_2.3.3      RGraphSpace_1.5.5 ggplot2_4.0.3    
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] sass_0.4.10        generics_0.1.4     tidyr_1.3.2        class_7.3-24      
    #>  [5] KernSmooth_2.23-27 lattice_0.23-1     digest_0.6.39      magrittr_2.0.5    
    #>  [9] evaluate_1.0.5     grid_4.6.1         RColorBrewer_1.1-3 fastmap_1.2.0     
    #> [13] jsonlite_2.0.0     Matrix_1.7-6       e1071_1.7-17       ggrastr_1.0.2     
    #> [17] DBI_1.3.0          purrr_1.2.2        scales_1.4.0       textshaping_1.0.5 
    #> [21] jquerylib_0.1.4    cli_3.6.6          rlang_1.3.0        units_1.0-1       
    #> [25] tidygraph_1.3.1    withr_3.0.3        cachem_1.1.0       yaml_2.3.12       
    #> [29] otel_0.2.0         ggbeeswarm_0.7.3   tools_4.6.1        dplyr_1.2.1       
    #> [33] vctrs_0.7.3        R6_2.6.1           proxy_0.4-29       classInt_0.4-11   
    #> [37] lifecycle_1.0.5    fs_2.1.0           htmlwidgets_1.6.4  vipor_0.4.7       
    #> [41] ragg_1.5.2         pkgconfig_2.0.3    beeswarm_0.4.0     desc_1.4.3        
    #> [45] pkgdown_2.2.0      pillar_1.11.1      bslib_0.11.0       gtable_0.3.6      
    #> [49] Rcpp_1.1.2         glue_1.8.1         sf_1.1-2           systemfonts_1.3.2 
    #> [53] xfun_0.59          tibble_3.3.1       tidyselect_1.2.1   rstudioapi_0.19.0 
    #> [57] knitr_1.51         dichromat_2.0-1    farver_2.1.2       htmltools_0.5.9   
    #> [61] rmarkdown_2.32     compiler_4.6.1     S7_0.2.2
