# Using 'sf' geometries with RGraphSpace

  

**Package**: RGraphSpace 1.5.3

``` r

# Check required version
if (packageVersion("RGraphSpace") < "1.5.2"){
  message("Need to update 'RGraphSpace' for this vignette")
  remotes::install_github("sysbiolab/RGraphSpace")
}
```

## Overview

## Setting basic input data

Below, we construct a star-like network with varying node sizes to show
how the geometries stay synchronized across a wide range values.

``` r

#--- Load packages
library("RGraphSpace")
library("igraph")
library("ggplot2")
library("sf")
```

``` r

# Make a toy graph
gtoy_star <- make_star(20, mode="out")

# Make a 'GraphSpace'
gs_star <- GraphSpace(gtoy_star, layout = layout_as_star(gtoy_star))

# Set node color and size
gs_star$nodeFillColor <- adjustcolor("blue", 0.1)
gs_star$nodeSize <- seq(1, gs_vcount(gs_star) )

# Normalize node coordinates
gs_star <- normalizeGraphSpace(gs_star)
```

The default node markers already reflect each node’s varying size:

``` r

# Plot with ggplot2 and RGraphSpace geoms
ggplot(gs_star) + 
  geom_edgespace() +
  geom_nodespace(colour = "red") + 
  theme_gspace_coords(is_norm = TRUE)
```

![](geometries_files/figure-html/Geometry%20-%202-1.png)

Next, build a set of decorative `sf` shapes, unrelated to the graph,
with no inherent size or position of their own:

``` r

# Make some decorative shapes
shapes20 <- c( sfshape_ngons(n = 10, sides = 3:7), 
  sfshape_stars(n = 10, points = 3:7) )

# Plot with ggplot2's geom_sf
ggplot(shapes20) + geom_sf() + 
  ggtitle("Decorative 'sf' geometries")
```

![](geometries_files/figure-html/Geometry%20-%203-1.png)

Attach the shapes to the nodes, then let
[`fitGeometry()`](https://sysbiolab.github.io/RGraphSpace/reference/geometry-methods.md)
position and size each one to match its node exactly:

``` r

# Attach the decorative shapes to the graph nodes
gs_geometry(gs_star, "geometry") <- shapes20

# Fit each shape to its node's size and position
gs_star <- fitGeometry(gs_star)
```

The geometries now track each node’s size and position precisely,
plotted here alongside the original markers for comparison:

``` r

# Plot nodes and fitted geometries
ggplot(gs_star) + 
  geom_edgespace() +
  geom_nodespace(colour = "red") +
  geom_sf(aes(geometry = geometry), fill = "blue") +
  theme_gspace_coords(is_norm = TRUE) +
  ggtitle("Decorative 'sf' geometries fit to nodes")
```

![](geometries_files/figure-html/Geometry%20-%205-1.png)

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
    #> [1] sf_1.1-1          igraph_2.3.3      RGraphSpace_1.5.3 ggplot2_4.0.3    
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
    #> [53] systemfonts_1.3.2  xfun_0.59          tibble_3.3.1       tidyselect_1.2.1  
    #> [57] rstudioapi_0.19.0  knitr_1.51         dichromat_2.0-1    farver_2.1.2      
    #> [61] htmltools_0.5.9    rmarkdown_2.31     compiler_4.6.1     S7_0.2.2
