# Building a Graph Space

  
**Package**: RGraphSpace 1.4.1

``` r

# Check required version
if (packageVersion("RGraphSpace") < "1.4.1"){
  message("Need to update 'RGraphSpace' for this vignette")
  remotes::install_github("sysbiolab/RGraphSpace")
}
```

## Quick start

This section will create a toy `igraph` object to demonstrate the basic
*RGraphSpace* workflow. The input data is configured manually to ensure
that users can easily view the basic graph attributes. We will use the
igraph’s [`make_star()`](https://r.igraph.org/reference/make_star.html)
function to create a simple star-like graph and then the
[`V()`](https://r.igraph.org/reference/V.html) and
[`E()`](https://r.igraph.org/reference/E.html) functions to set
attributes for vertices and edges, respectively. *RGraphSpace* will
require that all vertices have `x`, `y`, and `name` attributes.

``` r

#--- Load required packages
library("RGraphSpace")
library("igraph")
library("ggplot2")
```

``` r

# Make a 'toy' igraph with 5 nodes and 4 edges;
# ..either a directed or undirected graph
gtoy1 <- make_star(5, mode="out")

# Check whether the graph is directed or not
is_directed(gtoy1)
## [1] TRUE

# Check graph size
vcount(gtoy1)
## [1] 5
ecount(gtoy1)
## [1] 4

# Assign 'x' and 'y' coordinates to each vertex;
# ..this can be an arbitrary unit in (-Inf, +Inf)
V(gtoy1)$x <- c(0, 2, -2, -4, -8)
V(gtoy1)$y <- c(0, 0,  2, -4,  0)

# Assign a name to each vertex
V(gtoy1)$name <- paste0("n", 1:5)
```

``` r

# Plot the 'gtoy1' using standard R graphics
plot(gtoy1)
```

![](building-graphspace_files/figure-html/Toy%20igraph%20-%202-1.png)

``` r

# Plot the 'gtoy1' using RGraphSpace
plotGraphSpace(gtoy1, add.labels = TRUE)
```

![](building-graphspace_files/figure-html/Toy%20igraph%20-%203-1.png)

## *RGraphSpace* attributes

Next, we list all vertex and edge attributes that can be passed to
*RGraphSpace* methods.

### Vertex attributes

``` r

# Node size (numeric in [0, 100], as '%' of the plot space)
V(gtoy1)$nodeSize <- c(8, 5, 5, 10, 5)

# Node shape (integer code between 0 and 25; see 'help(points)')
V(gtoy1)$nodeShape <- c(21, 22, 23, 24, 25)

# Node color (Hexadecimal or color name)
V(gtoy1)$nodeColor <- c("red", "#00ad39", "grey80", "lightblue", "cyan")

# Node line width (as in 'lwd' standard graphics; see 'help(gpar)')
V(gtoy1)$nodeLineWidth <- 1

# Node line color (Hexadecimal or color name)
V(gtoy1)$nodeLineColor <- "grey20"

# Node labels ('NA' will omit labels)
V(gtoy1)$nodeLabel <- c("V1", "V2", "V3", "V4", "V5")

# Node label size (in pts)
V(gtoy1)$nodeLabelSize <- 8

# Node label color (Hexadecimal or color name)
V(gtoy1)$nodeLabelColor <- "black"

# Node transparency (in [0,1])
V(gtoy1)$nodeAlpha <- 1
```

### Edge attributes

Given a list of edges, *RGraphSpace* represents only one edge for each
pair of connected vertices. If there are multiple edges connecting the
same node pair, it will display the attributes of the first occurrence
in the data.

``` r

# Edge width (as in 'lwd' standard graphics; see 'help(gpar)')
E(gtoy1)$edgeLineWidth <- 0.8

# Edge color (Hexadecimal or color name)
E(gtoy1)$edgeLineColor <- c("red","green","blue","black")

# Edge type (as in 'lty' standard graphics; see 'help(gpar)')
E(gtoy1)$edgeLineType <- c("solid", "11", "dashed", "2124")

# Edge transparency (in [0,1])
E(gtoy1)$edgeAlpha <- 1
```

### Arrowhead attributes

**Arrowhead in directed graphs**: By default, an arrow will be drawn for
each edge according to its left-to-right orientation in the edge list
(*e.g.* `A -> B`). If there are mutual connections, the package will
recode the mutual edges to represent a bidirectional flow.

``` r

# Arrowhead types in directed graphs
## Integer or character code:
## 0 = "---", 1 = "-->", -1 = "--|"
E(gtoy1)$arrowType <- 1
```

**Arrowhead in undirected graphs**: By default, no arrow will be drawn
for undirected graphs. However, arrowheads may be assigned according to
the coding below.

``` r

# Arrowhead types in undirected graphs
## Integer or character code:
##  0 = "---"
##  1 = "-->",  2 = "<--",  3 = "<->",  4 = "|->",
## -1 = "--|", -2 = "|--", -3 = "|-|", -4 = "<-|", 
E(gtoy1)$arrowType <- 1
# Note: in undirected graphs, this attribute overrides 
# the edge's orientation in the edge list
```

… and plot the updated `igraph` object. The most straightforward call
simply passes an `igraph` directly to the wrapper
[`plotGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/plotGraphSpace-methods.md)
function.

``` r

# Plot the updated 'gtoy1' using RGraphSpace
plotGraphSpace(gtoy1, add.labels = TRUE)
```

![](building-graphspace_files/figure-html/A%20shortcut%20for%20RGraphSpace-1.png)

## Passing graphs to *geoms*

Alternatively, the `igraph` object can be transformed into a
`GraphSpace` container and passed to the underlying `geoms`:

``` r

data('gtoy1', package = 'RGraphSpace')
gs <- GraphSpace(gtoy1)
gs <- normalizeGraphSpace(gs)
ggplot(gs) + 
  geom_edgespace() + 
  geom_nodespace() + 
  geom_text(aes(x = x, y = y, label = nodeLabel), size = 2) + 
  theme_gspace_coords(is_norm = TRUE)
```

![](building-graphspace_files/figure-html/Using%20geoms-1.png)

For detailed integration with the *ggplot2* ecosystem and other spatial
packages, see [*customizing
aesthetics*](https://sysbiolab.github.io/RGraphSpace/articles/customizing-aesthetics.md)
and [*interoperability with ggraph &
sf*](https://sysbiolab.github.io/RGraphSpace/articles/interoperability.md)
vignettes.

## Session information

    #> R version 4.6.0 (2026-04-24)
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
    #> [1] igraph_2.3.2      RGraphSpace_1.4.1 ggplot2_4.0.3    
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] Matrix_1.7-5       gtable_0.3.6       jsonlite_2.0.0     dplyr_1.2.1       
    #>  [5] compiler_4.6.0     tidyselect_1.2.1   ggbeeswarm_0.7.3   tidyr_1.3.2       
    #>  [9] jquerylib_0.1.4    systemfonts_1.3.2  scales_1.4.0       textshaping_1.0.5 
    #> [13] yaml_2.3.12        fastmap_1.2.0      lattice_0.22-9     R6_2.6.1          
    #> [17] generics_0.1.4     knitr_1.51         htmlwidgets_1.6.4  tibble_3.3.1      
    #> [21] desc_1.4.3         bslib_0.11.0       pillar_1.11.1      RColorBrewer_1.1-3
    #> [25] rlang_1.2.0        cachem_1.1.0       xfun_0.58          fs_2.1.0          
    #> [29] sass_0.4.10        S7_0.2.2           otel_0.2.0         cli_3.6.6         
    #> [33] pkgdown_2.2.0      withr_3.0.2        magrittr_2.0.5     digest_0.6.39     
    #> [37] grid_4.6.0         beeswarm_0.4.0     lifecycle_1.0.5    vipor_0.4.7       
    #> [41] ggrastr_1.0.2      vctrs_0.7.3        evaluate_1.0.5     glue_1.8.1        
    #> [45] farver_2.1.2       ragg_1.5.2         tidygraph_1.3.1    purrr_1.2.2       
    #> [49] rmarkdown_2.31     tools_4.6.0        pkgconfig_2.0.3    htmltools_0.5.9
