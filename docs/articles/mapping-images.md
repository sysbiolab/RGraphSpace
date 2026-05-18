# Mapping graphs to images

  
**Package**: RGraphSpace 1.2.4

## Overview

Images can be used as spatial references for graphs. When a raster image
is provided, the pixel coordinates define where nodes are positioned,
supporting the construction of graphs from image features.

## Setting basic input data

As an example, next we extract features from the `volcano` matrix and
map them to graph nodes. The graph is then visualized over the image
matrix.

``` r

library("igraph")
library("ggplot2")
library("RGraphSpace")

# Extract pixel coordinates for a specific intensity quantile.
coords <- which(volcano == quantile(volcano, 0.85), arr.ind = TRUE)

# Mark target pixels with '0'; it will appear as black in the background. 
# This creates a visual anchor to verify the alignment precision.
volcano2 <- volcano
volcano2[coords] <- 0

# Create an igraph object from the pixel coordinates; 
# note that at this stage, 'y' represents matrix row indices.
gtoy3 <- igraph::make_empty_graph(n = nrow(coords))
igraph::V(gtoy3)$y <- coords[,1]
igraph::V(gtoy3)$x <- coords[,2]

# Highlight the bottom-row vertex (max 'y' index) to demonstrate alignment; 
# since matrix indexing is top-down, this accounts for the default flip 
# between matrix and plot coordinate systems.
igraph::V(gtoy3)$nodeColor <- NA
bottom_row <- which.max(igraph::V(gtoy3)$y)
igraph::V(gtoy3)$nodeColor[bottom_row] <- adjustcolor("red", 0.4)
```

## Initialize, normalize, and plot

``` r

# Initialize a GraphSpace object
gs <- GraphSpace(gtoy3)

# Map graph coordinates to the image space; by default,
# 'y' row indices will be flipped (see comments below).
gs <- normalizeGraphSpace(gs, image = as_colorraster(volcano2) )
```

Observe that the nodes (open circles) are precisely aligned with the
pixels (dark squares) to which they were mapped. In this plot, nodes
remain proportional to the viewport regardless of the final output
resolution. This behavior is especially critical when the network
elements represent image features at specific positions and must stay
anchored to those features, avoiding overlap with adjacent, unrelated
areas.

``` r

# Render the graph with the raster as background
plotGraphSpace(gs, add.image = TRUE)
```

![](mapping-images_files/figure-html/Mapping%20images%20-%203-1.png)

**Note on image alignment**: Proper spatial alignment between nodes and
the background image requires consistent coordinate conventions. Spatial
misalignment may occur if the input image and node coordinates differ in
axis orientation (e.g., top-left versus bottom-left origins). To
accommodate these differences,
[`normalizeGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/normalizeGraphSpace-methods.md)
provides orientation controls through the `rotate.xy`, `flip.x`, and
`flip.y` arguments. If the nodes appear misaligned with the input image,
try combinations of these parameters to correct the alignment.
Alternatively, try `flip.v` and `flip.h` arguments to apply flipping
directly to the background image.

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
    #> [1] RGraphSpace_1.2.4 ggplot2_4.0.3     igraph_2.3.1     
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] gtable_0.3.6       jsonlite_2.0.0     dplyr_1.2.1        compiler_4.6.0    
    #>  [5] tidyselect_1.2.1   ggbeeswarm_0.7.3   tidyr_1.3.2        jquerylib_0.1.4   
    #>  [9] systemfonts_1.3.2  scales_1.4.0       textshaping_1.0.5  yaml_2.3.12       
    #> [13] fastmap_1.2.0      R6_2.6.1           generics_0.1.4     knitr_1.51        
    #> [17] htmlwidgets_1.6.4  tibble_3.3.1       desc_1.4.3         bslib_0.10.0      
    #> [21] pillar_1.11.1      RColorBrewer_1.1-3 rlang_1.2.0        cachem_1.1.0      
    #> [25] xfun_0.57          fs_2.1.0           sass_0.4.10        S7_0.2.2          
    #> [29] otel_0.2.0         cli_3.6.6          withr_3.0.2        pkgdown_2.2.0     
    #> [33] magrittr_2.0.5     digest_0.6.39      grid_4.6.0         rstudioapi_0.18.0 
    #> [37] beeswarm_0.4.0     lifecycle_1.0.5    vipor_0.4.7        ggrastr_1.0.2     
    #> [41] vctrs_0.7.3        evaluate_1.0.5     glue_1.8.1         farver_2.1.2      
    #> [45] ragg_1.5.2         tidygraph_1.3.1    purrr_1.2.2        rmarkdown_2.31    
    #> [49] tools_4.6.0        pkgconfig_2.0.3    htmltools_0.5.9
