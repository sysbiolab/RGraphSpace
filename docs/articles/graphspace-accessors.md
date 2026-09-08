# GraphSpace accessors and transformations

  
**Package**: RGraphSpace 1.5.4

``` r

# Check required version
if (packageVersion("RGraphSpace") < "1.5.4"){
  message("Need to update 'RGraphSpace' for this vignette")
  remotes::install_github("sysbiolab/RGraphSpace")
}
```

## Overview

*RGraphSpace* is primarily designed for graph rendering and expects
graphs to be prepared before they enter the *RGraphSpace* workflow. More
extensive graph transformations is therefore generally best performed
upstream, while preparing the data used as input to *RGraphSpace*.

Nevertheless, downstream analyses may require accessing and modifying a
`GraphSpace` object to explore alternative graph configurations or
highlight specific subsets of the data.

In the following we demonstrate *RGraphSpace* accessors for graph
manipulations.

### Setting basic input data

``` r

#--- Load required packages
library("RGraphSpace")
library("igraph")
library("ggplot2")
```

``` r

# Load a demo igraph
data('gtoy1', package = 'RGraphSpace')

# Create a new GraphSpace object
gs <- GraphSpace(gtoy1)
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
gs_vertex_attr(gs)
#> $x
#> [1]  0  2 -2 -4 -8
#> 
#> $y
#> [1]  0  0  2 -4  0
#> 
#> $name
#> [1] "n1" "n2" "n3" "n4" "n5"
#> 
#> $nodeLabel
#> [1] "V1" "V2" "V3" "V4" "V5"
#> 
#> $nodeLabelSize
#> [1] 3 3 3 3 3
#> 
#> $nodeLabelColor
#> [1] "black" "black" "black" "black" "black"
#> 
#> $nodeShape
#> [1] 21 22 23 24 25
#> 
#> $nodeSize
#> [1]  8  5  5 10  5
#> 
#> $nodeFillColor
#> [1] "red"       "#00ad39"   "grey80"    "lightblue" "cyan"     
#> 
#> $nodeLineWidth
#> [1] 1 1 1 1 1
#> 
#> $nodeLineColor
#> [1] "grey20" "grey20" "grey20" "grey20" "grey20"
#> 
#> $nodeAlpha
#> [1] 1 1 1 1 1

# Access a specific vertex attribute
gs_vertex_attr(gs, "nodeLabel")
#>   n1   n2   n3   n4   n5 
#> "V1" "V2" "V3" "V4" "V5"

# Modify a single value within a vertex attribute
gs_vertex_attr(gs, "nodeSize")["n1"] <- 10

# Replace an entire vertex attribute
gs_vertex_attr(gs, "nodeSize") <- 10

# Add a new vertex attribute
gs_vertex_attr(gs, "new_node_var") <- rnorm(gs_vcount(gs))

# Delete a vertex attribute by assigning NULL
gs_vertex_attr(gs, "new_node_var") <- NULL

gs
#> A GraphSpace-class object for:
#> IGRAPH 5fb8aab DN-- 5 4 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeLabelSize
#> | (v/n), nodeLabelColor (v/c), nodeShape (v/n), nodeSize (v/n),
#> | nodeFillColor (v/c), nodeLineWidth (v/n), nodeLineColor (v/c),
#> | nodeAlpha (v/n), edgeLineType (e/c), edgeColor (e/c), edgeLineWidth
#> | (e/n), arrowType (e/n), edgeAlpha (e/n)
#> + node spatial boundaries: raw graph
#> | x: [-8, 2] (cols)
#> | y: [-4, 2] (rows)
```

### Edge attributes

``` r

# Access a specific edge attribute
gs_edge_attr(gs, "edgeColor")

# Replace an entire edge attribute
gs_edge_attr(gs, "edgeLineWidth") <- 1

# Add a new edge attribute
gs_edge_attr(gs, "new_edge_var") <- rnorm(gs_ecount(gs))

# Delete an edge attribute by assigning NULL
gs_edge_attr(gs, "new_edge_var") <- NULL

gs
```

## Adding nodes

The
[`gs_add_nodes()`](https://sysbiolab.github.io/RGraphSpace/reference/gs_add_nodes.md)
function adds one or more nodes to a `GraphSpace` object. Attributes
present on existing nodes but absent from value are filled from package
defaults. Standard node attributes (such as `nodeSize` and `nodeColor`)
are kept consistent across old and new nodes; the`@graph`, `@nodes`, and
`@fdata` slots are updated consistently. Because new nodes introduce
coordinates into the existing layout, the normalized state is
invalidated and
[`normalizeGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/normalizeGraphSpace-methods.md)
must be re-run afterwards.

``` r

# Functional form (pipe-friendly) returns a modified copy
gs <- gs |> gs_add_nodes(data.frame(name = "n6", x = 0.5, y = 0.5))

# Assignment form modifies gs in place
gs_add_nodes(gs) <- data.frame(name = "n7", x = 0.5, y = 0.5)

# Add multiple nodes with visual attributes
gs <- gs_add_nodes(gs, data.frame(
  name = c("n8", "n9"),
  x = c(0.5, 0.8),
  y = c(0.5, 0.2),
  nodeSize = c(8, 5),
  nodeColor = c("steelblue", "tomato")) )
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
gs <- gs |> gs_add_edges(data.frame(from = "n2", to = "n3"))

# Assignment form modifies gs in place
gs_add_edges(gs) <- data.frame(from = "n3", to = "n4")

# Add multiple edges with a numeric attribute
gs <- gs_add_edges(gs, data.frame(
  from   = c("n4", "n5"),
  to     = c("n5", "n6"),
  weight = c(0.8, 0.4)) )
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
```

## Subsetting edges

The
[`gs_subset_edges()`](https://sysbiolab.github.io/RGraphSpace/reference/gs_subset.md)
retains a subset of edges without modifying the node set, and the result
is propagated to all `GraphSpace` components.

``` r

# By predicate on an edge attribute
gs3 <- gs_subset_edges(gs, weight > 0.5)

# By endpoint names: name1 and name2 are columns in @edges and
# can be used directly inside any predicate expression
gs3 <- gs_subset_edges(gs, name1 == "n1")

# Combining endpoint and attribute conditions
gs3 <- gs_subset_edges(gs, name1 == "n1" & weight > 0.5)

# By integer position
gs3 <- gs_subset_edges(gs, 1:3)

# By logical vector
gs3 <- gs_subset_edges(gs, gs_edges(gs)$weight > 0.5)
```

## Subscript operators

The `[` operator subsets a `GraphSpace` object along two independent
dimensions: nodes (`i`) and edges (`j`). This differs from the usual
data-frame convention, where `[i, j]` indexes rows and columns of a
single table — here, `i` and `j` each address a distinct structural
component of the graph rather than rows and columns of one. Neither
index subsets columns; both select graph entities directly. Omitting an
index retains all elements along that dimension.

**Synchronization rules:**

- `x[i, ]` **Node-induced subgraph.** Nodes are selected by `i`, then
  edges are automatically pruned to those whose endpoints both survived.
  Normalized coordinates are preserved.
- `x[, j]` **Edge selection.** Edges are selected by `j`; the node set
  is untouched and no node pruning occurs.
- `x[i, j]` **Combined selection.** Node filtering is applied first,
  then `j` is then evaluated against the **original**, unfiltered edge
  table; an edge survives only if it satisfies `j` **and** both its
  endpoints survived node filtering.

``` r

# Node-induced subgraph: keep named nodes, prune dangling edges
gs[c("n1", "n2", "n3"), ]

# Node-induced subgraph by integer position
gs[1:4, ]

# Node-induced subgraph by pre-evaluated logical mask
gs[gs$nodeSize > 5, ]

# Edge selection only: keep all nodes
gs[, 1:3]
gs[, gs_edges(gs)$weight > 0.5]

# Edge selection by endpoint: 'name1' and 'name2' must be pre-evaluated
# when using [, because [ evaluates j in the calling environment.
# Use gs_subset_edges() for unquoted predicate expressions instead.
gs[, gs_edges(gs)$name1 == "n1"]
gs[, gs_edges(gs)$name1 == "n1" & gs_edges(gs)$name2 == "n2"]
gs[, quote(name1 == "n1" & name2 == "n2")]

# Combined: node filter first, then edge intersection
gs[c("n1", "n2", "n3"), gs_edges(gs)$weight > 0.5]
gs[c("n1", "n2", "n3"), gs_edges(gs)$name1 == "n1"]
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
tab-completion after `gs$` in interactive sessions (e.g. RStudio),
listing the vertex attributes currently defined on the graph.

``` r

# Return the vertex attribute `name`
gs$name

# Sets the vertex attribute `nodeShape`
gs$nodeShape <- 21
gs$nodeShape[1] <- 19

# Protected attributes cannot be modified this way
# gs$vertex <- 1  # error: 'vertex' is a read-only node attribute.
```

**Protected attributes**: those the package relies on internally
(e.g. node `name` and `vertex`) are not editable through `$<-`;
attempting to assign to one raises an error rather than silently
modifying it. Reading via `gs$` is unaffected.

## General accessors

All `GraphSpace` accessors start with a `gs_*` prefix to avoid
overlapping with other namespaces, especially other graph packages that
are often used alongside. Here we reproduce the general usage for these
accessors, already documented individually in the function help pages.

``` r

# Vertex names
names(gs)

# Vertex attribute names
gs_names(gs)

# Get a data frame with nodes
gs_nodes(gs)

# Get a data frame with edges
gs_edges(gs)

# Get an igraph object
gs_graph(gs)

# Get a data frame with nodes
gs_nodes(gs)

# Vertex count
gs_vcount(gs)

# Edge count
gs_ecount(gs)

# Add an image and rescale graph coordinates to image space
# Images may be provided as a raster or numeric matrix
gs_image(gs) <- as_colorraster(volcano)
gs <- normalizeGraphSpace(gs, image.space = FALSE)

# Add a sparse Matrix aligned to nodes
library(Matrix)
mtx <- Matrix(0, gs_vcount(gs), 2)
rownames(mtx) <- names(gs)
colnames(mtx) <- c("feature1","feature2")
gs_fdata(gs) <- mtx

# Feature names
gs_features(gs)

# Feature count
gs_nfeatures(gs)

# Apply a scaling factor to node coordinates
gs_scale_factor(gs) <- 0.1
# undo scaling
gs_scale_factor(gs) <- 1

# Add an 'sfc' geometry column (requires the optional 'sf' package)
if (requireNamespace("sf", quietly = TRUE)) {
  pts <- replicate(gs_vcount(gs), sf::st_point(runif(2)), simplify = FALSE)
  gs_geometry(gs) <- sf::st_sfc(pts)
}

# Set a pixel budget for image operations
gs_image_maxpixels(gs) <- 4e+06
```

## Applying *igraph* functions

The
[`gs_compute()`](https://sysbiolab.github.io/RGraphSpace/reference/gs_compute.md)
function runs any igraph function on the graph carried by a
`GraphSpace`, without needing a dedicated `gs_*` wrapper for each one.
It extracts the underlying igraph via
[`as.igraph()`](https://r.igraph.org/reference/as.igraph.html), applies
`.f`, and returns the result unchanged. This is the read-only lane onto
the whole igraph ecosystem: measures such as
[`degree()`](https://r.igraph.org/reference/degree.html),
[`betweenness()`](https://r.igraph.org/reference/betweenness.html),
[`coreness()`](https://r.igraph.org/reference/coreness.html), and
distances all work through this one entry point.

``` r

# Apply igraph functions
gs_compute(gs, igraph::degree)
gs_compute(gs, "betweenness", directed = FALSE)

# Fold a per-vertex result back as a node attribute
gs$degree <- gs_compute(gs, igraph::degree)
```

It is deliberately *not* a graph-modification path. If `.f` returns a
graph
(e.g. [`simplify()`](https://r.igraph.org/reference/simplify.html),
[`induced_subgraph()`](https://r.igraph.org/reference/subgraph.html)),
this cannot be reintegrated to `gs` as a modified graph must go through
the graph-modification checks.

## Crop, rotate, flip, and transpose

These functions are special accessors, as they operate on a reference
frame, either the graph or image spaces, not on the graph alone. Node
and edge attributes, and the underlying `igraph` object, are left
untouched (aside from cropping’s node/edge dropping, which follows from
the region no longer containing them).

- [`cropGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-transform.md)
  subsets a normalized `GraphSpace` to a specific region defined by the
  cropping boundaries. It recalculates node positions and background
  image boundaries to maintain spatial consistency after cropping, and
  drops nodes (and edges) that fall outside the window.
- [`rotateGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-transform.md),
  [`flipGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-transform.md),
  and
  [`transposeGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-transform.md)
  are all exact — a coordinate/pixel permutation, with no resampling, no
  interpolation, and no risk of misaligning nodes against the background
  image. When these transformations are applied to a normalized
  `GraphSpace`, the changes are reversible. Otherwise they are applied
  to raw coordinates.

``` r

# Create a GraphSpace
gs <- GraphSpace(make_full_graph(30))

# Normalize coordinates
gs <- normalizeGraphSpace(gs)

# Crop
gs_crop <- cropGraphSpace(gs, ymax = 0.5)

# Rotate
gs_rot90 <- rotateGraphSpace(gs)

# Flip
gs_flip <- flipGraphSpace(gs)

# Transpose
gs_t <- transposeGraphSpace(gs)
```

## Session information

    #> R version 4.6.1 (2026-06-24)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Ubuntu 24.04.4 LTS
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
    #> [1] igraph_2.3.3      RGraphSpace_1.5.4 ggplot2_4.0.3    
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] Matrix_1.7-6       gtable_0.3.6       jsonlite_2.0.0     dplyr_1.2.1       
    #>  [5] compiler_4.6.1     tidyselect_1.2.1   ggbeeswarm_0.7.3   dichromat_2.0-1   
    #>  [9] tidyr_1.3.2        jquerylib_0.1.4    systemfonts_1.3.2  scales_1.4.0      
    #> [13] textshaping_1.0.5  yaml_2.3.12        fastmap_1.2.0      lattice_0.23-1    
    #> [17] R6_2.6.1           generics_0.1.4     knitr_1.51         htmlwidgets_1.6.4 
    #> [21] tibble_3.3.1       desc_1.4.3         bslib_0.11.0       pillar_1.11.1     
    #> [25] RColorBrewer_1.1-3 rlang_1.3.0        cachem_1.1.0       xfun_0.59         
    #> [29] fs_2.1.0           sass_0.4.10        S7_0.2.2           otel_0.2.0        
    #> [33] cli_3.6.6          pkgdown_2.2.0      withr_3.0.3        magrittr_2.0.5    
    #> [37] digest_0.6.39      grid_4.6.1         rstudioapi_0.19.0  beeswarm_0.4.0    
    #> [41] lifecycle_1.0.5    vipor_0.4.7        ggrastr_1.0.2      vctrs_0.7.3       
    #> [45] evaluate_1.0.5     glue_1.8.1         farver_2.1.2       ragg_1.5.2        
    #> [49] tidygraph_1.3.1    purrr_1.2.2        rmarkdown_2.32     tools_4.6.1       
    #> [53] pkgconfig_2.0.3    htmltools_0.5.9
