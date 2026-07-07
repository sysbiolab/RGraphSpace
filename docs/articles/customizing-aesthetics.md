# Customizing Aesthetics

  
**Package**: RGraphSpace 1.4.2

``` r

# Check required version
if (packageVersion("RGraphSpace") < "1.4.2"){
  message("Need to update 'RGraphSpace' for this vignette")
  remotes::install_github("sysbiolab/RGraphSpace")
}
```

## Overview

This section illustrates how *RGraphSpace* integrates with *ggplot2*
using `geoms` building blocks (Wickham 2016). Graph attributes stored in
the `GraphSpace` object can be handled in two ways:

- **Identity mapping:** Attributes such as `nodeColor`, `nodeSize`, and
  `nodeShape` are treated as literal values and displayed exactly as
  specified, without scaling or transformation.

- **Dynamic aesthetic mapping:** Attributes are mapped to *ggplot2*
  aesthetics such as `colour`, `size`, and `shape`, and rendered through
  standard scales, which automatically generate synchronized legends.

*RGraphSpace* implements two specialized `geoms` for handling graph data
within a *ggplot2* workflow. Both use a dual-anchor normalization
strategy to keep layers aligned, which is essential when network
elements must remain accurately referenced to a spatial map or image.

1.  **[`geom_nodespace()`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md)**:
    Renders network nodes. Extends `GeomPoint` aesthetic mappings and
    exposes node state information to the edge layer.
2.  **[`geom_edgespace()`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)**:
    Renders the relationships between nodes. Extends `GeomSegment`
    aesthetic mappings; unlike standard segments, it is node-aware and
    dynamically adjusts start and end points based on node position and
    size.

## Setting basic input data

In the following example, we create a small modular graph containing
variables of different types to demonstrate how *RGraphSpace* `geoms`
handle different mapping requirements.

``` r

#--- Load packages
library("RGraphSpace")
library("igraph")
library("ggplot2")
```

``` r

# Make a toy modular graph
gtoy2 <- sample_islands(
  islands.n = 3,       # number of modules
  islands.size = 30,   # nodes per module
  islands.pin = 0.25,  # probability of edges within modules
  n.inter = 2)         # edges between modules

# Assign module membership to nodes
V(gtoy2)$module <- rep(1:3, each = 30)

# Assign colors to nodes
V(gtoy2)$nodeColor <- rainbow(3)[V(gtoy2)$module]

# Assign a categorical variable to nodes
V(gtoy2)$node_group <- c("A", "B", "C")[V(gtoy2)$module]

# Assign numeric variables to nodes and edges
V(gtoy2)$node_var <- runif(vcount(gtoy2))
E(gtoy2)$edge_var <- runif(ecount(gtoy2))

# Create a GraphSpace from the toy igraph
gs <- GraphSpace(gtoy2)
#> Validating the 'igraph' object...
#> Vertex attributes 'x' and 'y' missing; computing layout...
#> Vertex attribute 'name' missing; assigning names... 
#> Ignoring graph-level attributes: 'name', 'islands_n', 'islands_size', ...
#> Creating a 'GraphSpace' object...

gs
#> A GraphSpace-class object for:
#> IGRAPH 8dda1cd UN-- 90 340 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n),
#> | nodeColor (v/c), module (v/n), node_group (v/c), node_var (v/n),
#> | arrowType (e/n), edge_var (e/n)
#> + node spatial boundaries: raw graph
#> | x: [-10, 8] (cols)
#> | y: [-9, 8] (rows)
```

## Plotting identity values

In this example, `nodeColor` already contains the final colour values
stored in the `GraphSpace` object. The colours will be displayed as-is
by the
[`geom_nodespace()`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md)
function. This approach is useful when nodes have been pre-processed
with specific attributes and you want the visual output without further
mapping.

``` r

ggplot(gs) + 
  geom_edgespace() +
  geom_nodespace(colour = "grey") +
  theme(aspect.ratio = 1)
```

![](customizing-aesthetics_files/figure-html/Plot%20identity%20values-1.png)

The trade-off on this approach is that, on one hand, all attributes
reflect the original data directly, but no legend is accessible. This is
because identity scales bypass the legend-building process of *ggplot2*.
If a legend is required to explain the meaning of these colors, the
attribute should be mapped via aesthetics (e.g.,
`aes(fill = attribute)`) and modified by a `scale_*()` function.

## Mapping categorical variables

In this example, the node categorical variable `node_group` is mapped to
the `fill` aesthetic.

``` r

ggplot(gs) + 
  geom_edgespace() +
  geom_nodespace(aes(fill = node_group), colour = "grey") +
  scale_fill_viridis_d(option = "viridis") +
  theme_gspace_coords()
```

![](customizing-aesthetics_files/figure-html/Map%20aesthetics%20to%20categorical%20variables-1.png)

## Mapping numeric variables

In this example, node and edge numeric variables are mapped to `fill`
and `colour` aesthetics, respectively.

``` r

# Map aesthetics to numeric variables
ggplot(gs) + 
  geom_edgespace(aes(colour = edge_var)) +
  geom_nodespace(aes(fill = node_var), colour = "grey") +
  scale_colour_continuous(palette = c("cyan","blue")) +
  scale_fill_continuous(palette = c("white","purple")) +
  theme_gspace_coords()
```

![](customizing-aesthetics_files/figure-html/Map%20aesthetics%20to%20numeric%20variables-1.png)

## Using separate colour scales

If you need to map different variables to the same aesthetic (such as
`colour`) with independent scales, the *ggnewscale* package offers an
elegant solution to introduce additional scales within the same plot
(Campitelli 2025); for example:

``` r

if (!require("ggnewscale", quietly = TRUE)) {
  install.packages("ggnewscale")
}
library("ggnewscale")
ggplot(data = gs) + 
  geom_edgespace(aes(colour = edge_var)) +
  scale_colour_continuous(palette = c("cyan","blue")) +
  ggnewscale::new_scale_colour() +
  geom_nodespace(aes(colour = node_var), 
    stroke = 2, fill = NA) +
  scale_colour_continuous(palette = c("white","purple")) +
  theme_gspace_coords()
```

![](customizing-aesthetics_files/figure-html/Map%20aesthetics%20to%20separate%20colour%20scales-1.png)

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
    #> [1] ggnewscale_0.5.2  igraph_2.3.3      RGraphSpace_1.4.2 ggplot2_4.0.3    
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] sass_0.4.10        generics_0.1.4     tidyr_1.3.2        lattice_0.22-9    
    #>  [5] digest_0.6.39      magrittr_2.0.5     evaluate_1.0.5     grid_4.6.1        
    #>  [9] RColorBrewer_1.1-3 fastmap_1.2.0      jsonlite_2.0.0     Matrix_1.7-5      
    #> [13] ggrastr_1.0.2      purrr_1.2.2        viridisLite_0.4.3  scales_1.4.0      
    #> [17] textshaping_1.0.5  jquerylib_0.1.4    cli_3.6.6          rlang_1.2.0       
    #> [21] tidygraph_1.3.1    withr_3.0.3        cachem_1.1.0       yaml_2.3.12       
    #> [25] otel_0.2.0         ggbeeswarm_0.7.3   tools_4.6.1        dplyr_1.2.1       
    #> [29] vctrs_0.7.3        R6_2.6.1           lifecycle_1.0.5    fs_2.1.0          
    #> [33] htmlwidgets_1.6.4  vipor_0.4.7        ragg_1.5.2         pkgconfig_2.0.3   
    #> [37] beeswarm_0.4.0     desc_1.4.3         pkgdown_2.2.0      pillar_1.11.1     
    #> [41] bslib_0.11.0       gtable_0.3.6       glue_1.8.1         systemfonts_1.3.2 
    #> [45] xfun_0.59          tibble_3.3.1       tidyselect_1.2.1   rstudioapi_0.19.0 
    #> [49] knitr_1.51         farver_2.1.2       htmltools_0.5.9    rmarkdown_2.31    
    #> [53] labeling_0.4.3     compiler_4.6.1     S7_0.2.2

## References

Campitelli, Elio. 2025. *Ggnewscale: Multiple Fill and Colour Scales in
’Ggplot2’*. <https://doi.org/10.32614/CRAN.package.ggnewscale>.

Wickham, Hadley. 2016. *Ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York. <https://ggplot2.tidyverse.org>.
