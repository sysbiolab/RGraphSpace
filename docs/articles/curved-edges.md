# Curved Edges, Parallel Edges, and Self-Loops

``` r

# Check required version
if (packageVersion("RGraphSpace") < "1.5.2"){
  message("Need to update 'RGraphSpace' for this vignette")
  remotes::install_github("sysbiolab/RGraphSpace")
}
```

## Overview

The
[`geom_edgespace()`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)
function can render edges as curves through its `curve` argument. Curved
edges are especially useful when a pair of nodes is connected by more
than one edge, or when an edge starts and ends at the same node (a
self-loop).

## Setting up the example data

We build a small toy graph containing ordinary edges, parallel edges,
and self-loops. A self-loop is an edge whose two endpoints are the same
vertex, whereas parallel edges are edges that connect the same pair of
vertices. The graph is wrapped into `GraphSpace` objects with
`simplify = FALSE`, since parallel edges and self-loops are otherwise
removed by default when a graph is validated.

``` r

# Load packages
library("RGraphSpace")
library("igraph")
library("ggplot2")
```

``` r

# Toy graph topology: hub + ring
n <- 10; hub <- 5
g <- make_ring(n, directed = TRUE) %u% 
  make_star(n, center = hub, mode = "out")
  
# Add self-loops
g <- add_edges(g, rep(c(2,4,6,8,10,10), each = 2))

# Add parallel edges
g <- add_edges(g, c(6,5, 6,5, 5,4))

# Set node and edge colors via igraph attributes directly, rather than
# through a geom, as these values will be fixed (not data-driven)
V(g)$nodeColor <- "steelblue1"
E(g)$edgeColor <- "purple"
E(g)$label <- seq_len(ecount(g))
  
# Build a GraphSpace with a circular layout; simplify = FALSE keeps the
# parallel edges and loops intact
gs_toy <- GraphSpace(g, layout = layout_in_circle(g), simplify = FALSE)
#> Validating the 'igraph' object...
#> Vertex attribute 'name' missing; assigning names... 
#> Ignoring graph-level attributes: 'name_1', 'name_2', 'mutual', ...
#> Creating a 'GraphSpace' object...

# Rescale coordinates to a standard range 
# (optional, but recommended for consistent plotting)
gs_toy <- normalizeGraphSpace(gs_toy)
#> Normalizing node coordinates to graph space...

gs_toy
#> A GraphSpace-class object for:
#> IGRAPH a471a30 DN-- 10 27 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | nodeColor (v/c), edgeColor (e/c), arrowType (e/n), label (e/n)
#> + node spatial boundaries: normalized to graph space
#> | x: [-1, 1] -> [0, 1] (cols)
#> | y: [-1, 1] -> [0, 1] (rows)
```

## Curved edges

By default, simple edges are drawn as straight lines.

``` r

ggplot(gs_toy) +
  geom_edgespace(aes(label = label), label_size = 2) +
  geom_nodespace(aes(label = nodeLabel), label_size = 3) +
  theme_gspace_coords(is_norm = TRUE)
```

![](curved-edges_files/figure-html/Straight%20edges-1.png)

Increasing `curve` bows simple edges away from a straight line.
Arrowheads, labels, and node-clipping offsets automatically follow the
local `curve` direction, so they stay correctly oriented as the edge
bends.

``` r

ggplot(gs_toy) +
  geom_edgespace(aes(label = label), label_size = 2, curve = 0.2) +
  geom_nodespace(aes(label = nodeLabel), label_size = 3) +
  theme_gspace_coords(is_norm = TRUE)
```

![](curved-edges_files/figure-html/Gentle%20curve-1.png)

The sign of `curve` sets which side the simple edge bows toward.

``` r

ggplot(gs_toy) +
  geom_edgespace(aes(label = label), label_size = 2, curve = -0.2) +
  geom_nodespace(aes(label = nodeLabel), label_size = 3) +
  theme_gspace_coords(is_norm = TRUE)
```

![](curved-edges_files/figure-html/Curve%20sign%20flips%20the%20side-1.png)

## Parallel edges and self-loops

`curve` applies uniformly to simple edges in a layer, which is not
enough on its own to keep parallel edges apart, or to make a self-loop
visible (a loop has no length for `curve` to be a fraction of).
*RGraphSpace* handles both automatically: edges that share the same pair
of nodes fan out, and self-loops are drawn with their own geometry – all
without any extra arguments.

The amount of fan-out is controlled by a separate argument,
`parallel_spread` (default `1`), independently of `curve`. Lower values
keep parallel edges and loops closer together; higher values spread them
further apart.

``` r

ggplot(gs_toy) +
  geom_edgespace(aes(label = label), label_size = 2,
    curve = 0.3, parallel_spread = 2) +
  geom_nodespace(aes(label = nodeLabel), label_size = 3) +
  theme_gspace_coords(is_norm = TRUE)
```

![](curved-edges_files/figure-html/Curve%20and%20parallel_spread%20together-1.png)

## Orienting self-loops

Self-loops also accept a `loop_direction` argument, which controls where
a loop sits relative to its node, independently of `parallel_spread`’s
control over its size. The default, `"adaptive"`, points each loop away
from the graph’s centroid, so loops naturally expand toward less crowded
regions of the layout. A single numeric value fixes every loop at the
same angle (in degrees), regardless of node position.

``` r

ggplot(gs_toy) +
  geom_edgespace(aes(label = label), label_size = 2, 
    curve = 0.3, parallel_spread = 2, loop_direction = 90) +
  geom_nodespace(aes(label = nodeLabel), label_size = 2) +
  theme_gspace_coords(is_norm = TRUE)
```

![](curved-edges_files/figure-html/Adaptive%20loop%20direction-1.png)

## Session information

``` r

sessionInfo()
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
#> [1] igraph_2.3.3      RGraphSpace_1.5.3 ggplot2_4.0.3    
#> 
#> loaded via a namespace (and not attached):
#>  [1] tidyr_1.3.2        sass_0.4.10        generics_0.1.4     class_7.3-24      
#>  [5] KernSmooth_2.23-27 lattice_0.23-1     digest_0.6.39      magrittr_2.0.5    
#>  [9] evaluate_1.0.5     grid_4.6.1         RColorBrewer_1.1-3 fastmap_1.2.0     
#> [13] Matrix_1.7-6       jsonlite_2.0.0     e1071_1.7-17       ggrastr_1.0.2     
#> [17] DBI_1.3.0          purrr_1.2.2        scales_1.4.0       codetools_0.2-20  
#> [21] textshaping_1.0.5  jquerylib_0.1.4    cli_3.6.6          rlang_1.3.0       
#> [25] units_1.0-1        tidygraph_1.3.1    withr_3.0.3        cachem_1.1.0      
#> [29] yaml_2.3.12        otel_0.2.0         ggbeeswarm_0.7.3   tools_4.6.1       
#> [33] dplyr_1.2.1        vctrs_0.7.3        R6_2.6.1           proxy_0.4-29      
#> [37] lifecycle_1.0.5    classInt_0.4-11    fs_2.1.0           htmlwidgets_1.6.4 
#> [41] vipor_0.4.7        ragg_1.5.2         pkgconfig_2.0.3    beeswarm_0.4.0    
#> [45] desc_1.4.3         terra_1.9-34       pkgdown_2.2.0      pillar_1.11.1     
#> [49] bslib_0.11.0       gtable_0.3.6       Rcpp_1.1.2         glue_1.8.1        
#> [53] sf_1.1-1           systemfonts_1.3.2  xfun_0.59          tibble_3.3.1      
#> [57] tidyselect_1.2.1   rstudioapi_0.19.0  knitr_1.51         dichromat_2.0-1   
#> [61] farver_2.1.2       htmltools_0.5.9    rmarkdown_2.31     compiler_4.6.1    
#> [65] S7_0.2.2
```
