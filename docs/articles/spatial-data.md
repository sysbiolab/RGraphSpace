# Using RGraphSpace with Spatial Feature Data

**Package**: RGraphSpace 1.5.3  

## Overview

This vignette demonstrates how *RGraphSpace* renders spatial
transcriptomics data. Using spatial data from the *SeuratData* package,
we show how a graph is overlaid on a reference tissue image and
high-dimensional features are mapped to *ggplot2* aesthetics through the
`GraphSpace` interface.

## Before you start

This vignette assumes familiarity with
[*Seurat*](https://satijalab.org/seurat/) (Hao et al. 2024),
particularly for handling spatial transcriptomics data.

![](data:image/svg+xml;base64,PHN2ZyBhcmlhLWhpZGRlbj0idHJ1ZSIgcm9sZT0iaW1nIiB2aWV3Ym94PSIwIDAgNTEyIDUxMiIgc3R5bGU9ImhlaWdodDoxZW07d2lkdGg6MWVtO3ZlcnRpY2FsLWFsaWduOi0wLjEyNWVtO21hcmdpbi1sZWZ0OmF1dG87bWFyZ2luLXJpZ2h0OmF1dG87Zm9udC1zaXplOmluaGVyaXQ7ZmlsbDpvcmFuZ2U7b3ZlcmZsb3c6dmlzaWJsZTtwb3NpdGlvbjpyZWxhdGl2ZTsiPjxwYXRoIGQ9Ik0yNTYgMzJjMTQuMiAwIDI3LjMgNy41IDM0LjUgMTkuOGwyMTYgMzY4YzcuMyAxMi40IDcuMyAyNy43IC4yIDQwLjFTNDg2LjMgNDgwIDQ3MiA0ODBINDBjLTE0LjMgMC0yNy42LTcuNy0zNC43LTIwLjFzLTctMjcuOCAuMi00MC4xbDIxNi0zNjhDMjI4LjcgMzkuNSAyNDEuOCAzMiAyNTYgMzJ6bTAgMTI4Yy0xMy4zIDAtMjQgMTAuNy0yNCAyNFYyOTZjMCAxMy4zIDEwLjcgMjQgMjQgMjRzMjQtMTAuNyAyNC0yNFYxODRjMC0xMy4zLTEwLjctMjQtMjQtMjR6bTMyIDIyNGEzMiAzMiAwIDEgMCAtNjQgMCAzMiAzMiAwIDEgMCA2NCAweiIgLz48L3N2Zz4=)**Note:**
If you are new to *Seurat*, we recommend reviewing its [spatial analysis
tutorials](https://satijalab.org/seurat/articles/spatial_vignette)
before proceeding.

**Computational requirement:**

- Hardware: RAM \>= 16 GB

- Software: R (\>=4.5) and RStudio

## Required packages

![](data:image/svg+xml;base64,PHN2ZyBhcmlhLWhpZGRlbj0idHJ1ZSIgcm9sZT0iaW1nIiB2aWV3Ym94PSIwIDAgNTEyIDUxMiIgc3R5bGU9ImhlaWdodDoxZW07d2lkdGg6MWVtO3ZlcnRpY2FsLWFsaWduOi0wLjEyNWVtO21hcmdpbi1sZWZ0OmF1dG87bWFyZ2luLXJpZ2h0OmF1dG87Zm9udC1zaXplOmluaGVyaXQ7ZmlsbDpvcmFuZ2U7b3ZlcmZsb3c6dmlzaWJsZTtwb3NpdGlvbjpyZWxhdGl2ZTsiPjxwYXRoIGQ9Ik0yNTYgMzJjMTQuMiAwIDI3LjMgNy41IDM0LjUgMTkuOGwyMTYgMzY4YzcuMyAxMi40IDcuMyAyNy43IC4yIDQwLjFTNDg2LjMgNDgwIDQ3MiA0ODBINDBjLTE0LjMgMC0yNy42LTcuNy0zNC43LTIwLjFzLTctMjcuOCAuMi00MC4xbDIxNi0zNjhDMjI4LjcgMzkuNSAyNDEuOCAzMiAyNTYgMzJ6bTAgMTI4Yy0xMy4zIDAtMjQgMTAuNy0yNCAyNFYyOTZjMCAxMy4zIDEwLjcgMjQgMjQgMjRzMjQtMTAuNyAyNC0yNFYxODRjMC0xMy4zLTEwLjctMjQtMjQtMjR6bTMyIDIyNGEzMiAzMiAwIDEgMCAtNjQgMCAzMiAzMiAwIDEgMCA2NCAweiIgLz48L3N2Zz4=)
Before proceeding, ensure that all packages described in the
[*Installation
Instructions*](https://sysbiolab.github.io/RGraphSpace/articles/install.md)
are installed.

``` r

# Check versions
if (packageVersion("RGraphSpace") < "1.5.2"){
  message("Need to update 'RGraphSpace' for this vignette")
  remotes::install_github("sysbiolab/RGraphSpace")
}
if (packageVersion("Seurat") < "5.5.1"){
  message("Need to update 'Seurat' for this vignette")
  remotes::install_github("satijalab/Seurat")
}
```

## Setting input data

``` r

# Load packages
library("RGraphSpace")
library("Seurat")
library("SeuratObject")
library("SeuratData")
```

### Loading the dataset

We will use the `stxBrain` dataset from the *SeuratData* package,
consisting of spatial transcriptomics data from sagittal mouse brain
sections generated with Visium v1 technology. This dataset is commonly
used to demonstrate *Seurat* spatial workflows (Hao et al. 2024). We
apply
[`as.GraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/as.GraphSpace.md)
to coerce the `Seurat` object into a `GraphSpace` and show how its
spatial features can be mapped to *ggplot2* aesthetics, anchored to the
tissue image from which the data were sampled.

``` r

# Install a Seurat dataset (required only once)
SeuratData::InstallData("stxBrain")
```

``` r

# Check manifest of installed datasets
# SeuratData::InstalledData()

# Load the 'stxBrain' dataset
# Note: LoadData() may print conversion warnings when loading pbmc3k.
# These are expected and come from SeuratData's internal v4-to-v5
# object migration — they can be safely ignored.
seurat_obj <- LoadData("stxBrain", type = "anterior1")
```

### Preprocessing

The `stxBrain` dataset is normalized as suggested in *Seurat*’s
[spatial_vignette](https://satijalab.org/seurat/articles/spatial_vignette),
either using the
[`SCTransform()`](https://satijalab.org/seurat/reference/SCTransform.html)
and
[`NormalizeData()`](https://satijalab.org/seurat/reference/NormalizeData.html)
functions.

``` r

# NOTE: Seurat recommends using SCTransform() for processing this 
# spatial dataset, which may require more computation time. Here,
# we use log-normalization for demonstration purposes.
seurat_obj <- NormalizeData(seurat_obj)
```

### Creating a GraphSpace object

Next, we create a `GraphSpace` from the `Seurat` object;
[`as.GraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/as.GraphSpace.md)
brings its spatial coordinates and feature data into the `GraphSpace`,
making them available for aesthetic mapping. We then attach the tissue
image and normalize node coordinates to the image space.

``` r

# Create a GraphSpace from 'seurat_obj'
gs <- as.GraphSpace(seurat_obj, space = "spatial", scale = "lowres")
#> Seurat object converted to GraphSpace:
#> ℹ space=spatial, layer=default, features=31053, samples=2696, scale="lowres"
#> Node spatial boundaries:
#> ℹ x: [76, 493] (cols)
#> ℹ y: [138, 541] (rows)
```

``` r

# If available, add tissue image 
gs_image(gs) <- SeuratObject::GetImage(seurat_obj, mode = "raster")
#> Image spatial boundaries:
#> ℹ x: [1, 600] (cols)
#> ℹ y: [1, 599] (rows)
```

``` r

# Normalize node coordinates to the image space
gs <- normalizeGraphSpace(gs)
#> Normalizing node coordinates to image space...
#> Flipping y-coordinates...

gs
#> A GraphSpace-class object for:
#> IGRAPH 6fd732c UN-- 2696 0 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeSize (v/n), cell (v/c),
#> | orig.ident (v/x), nCount_Spatial (v/n), nFeature_Spatial (v/n), slice (v/n), region
#> | (v/c), arrowType (e/n)
#> + features: 31053 (Xkr4, Gm1992, Gm37381, Rp1, ...)
#> + samples: 2696 (AAACAAGTATCTCCCA-1, AAACACCAATAACTGC-1, ...)
#> + node spatial boundaries: normalized to image space
#> | x: [76, 493] -> [0, 1] (cols)
#> | y: [138, 541] -> [0, 1] (rows)
#> + image spatial boundaries: cropped to graph space
#> | x: [1, 600] -> [1, 522] (cols)
#> | y: [1, 599] -> [1, 522] (rows)
```

## Spatial feature visualization

With the `GraphSpace` object ready, we can reproduce a spatial feature
plot of the kind familiar from *Seurat*, using standard *ggplot2*
mappings. Here we map expression of the `Ttr` gene to the `colour`
aesthetic and display the tissue image as a background reference.

``` r

cpal <- hcl.colors(100, palette = "Spectral", rev = TRUE)

# Reproduce a typical Seurat's spatial feature visualization
ggplot(gs) + 
  annotation_gspace_image(gs) +
  geom_nodespace(mapping = aes(colour = Ttr), size = 1, pch = 19) +
  scale_colour_continuous(palette = cpal) +
  theme_gspace_coords(theme = "th3", is_norm = TRUE, 
    xlab = "Tissue coordinates 1", ylab = "Tissue coordinates 2")
```

![](figs_dev/ggplot_seurat_3.png)

**Note on image alignment**: Proper spatial alignment between nodes and
the background image requires consistent coordinate conventions. Spatial
misalignment may occur if the input image and node coordinates differ in
axis orientation (e.g., top-left versus bottom-left origins). To
accommodate these differences,
[`normalizeGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/normalizeGraphSpace-methods.md)
provides orientation controls through the `swap.xy`, `flip.x`, and
`flip.y` arguments. If the nodes appear misaligned with the input image,
try combinations of these parameters to correct the alignment.
Alternatively, try `flip.v` and `flip.h` arguments to apply flipping
directly to the background image.

## Spatial cluster visualization

This section requires additional preprocessing of the `stxBrain`
dataset, including normalization with
[`SCTransform()`](https://satijalab.org/seurat/reference/SCTransform.html)
and Seurat’s clustering workflow. We recommend installing the
*glmGamPoi* package beforehand, as it substantially speeds up the
[`SCTransform()`](https://satijalab.org/seurat/reference/SCTransform.html)
estimation step.

### Preprocessing

``` r

if (!require("glmGamPoi", quietly = TRUE)){
  BiocManager::install("glmGamPoi")
}
# Run vst normalization on counts
seurat_obj <- SCTransform(seurat_obj, assay = "Spatial", verbose = FALSE)
seurat_obj <- RunPCA(seurat_obj, assay = "SCT", verbose = FALSE)
seurat_obj <- FindNeighbors(seurat_obj, reduction = "pca", dims = 1:30)
seurat_obj <- FindClusters(seurat_obj, verbose = FALSE)
```

### Spatial cluster visualization

With clusters assigned, we rebuild the `GraphSpace` object from the
updated `seurat_obj` and reproduce a spatial cluster plot, mapping
cluster identity to the `fill` aesthetic and overlaying the tissue image
as a dimmed background.

``` r

# Re-create a GraphSpace from the updated 'seurat_obj'
gs <- as.GraphSpace(seurat_obj, space = "spatial", scale = "lowres")
gs_image(gs) <- SeuratObject::GetImage(seurat_obj, mode = "raster")
gs <- normalizeGraphSpace(gs)
```

``` r

# Reproduce a typical Seurat cluster visualization
cpal <- DiscretePalette(nlevels(gs$seurat_clusters), palette = "polychrome")
ggplot(gs) + 
  annotation_gspace_image(gs, opacity = 0.5) +
  geom_nodespace(mapping = aes(fill = seurat_clusters),
    size = 1.3, color = "grey90", stroke = 0.3) +
  scale_fill_manual(values = cpal) +
  theme_gspace_coords(theme = "th2", is_norm = TRUE, 
    xlab = "Tissue coordinates 1", ylab = "Tissue coordinates 2") +
  theme_gspace_legend(discrete_fill = TRUE)
```

![](figs_dev/ggplot_seurat_4.png)

  

## Coercing *Seurat* spatial objects

Below, we show how to access the relevant components of a `Seurat`
object and use them to construct a `GraphSpace` manually, without
relying on
[`as.GraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/as.GraphSpace.md).
For another coercion example, see the [*high-dimensional
data*](https://sysbiolab.github.io/RGraphSpace/articles/high-dimensional.html#hd-coercion)
tutorial.

``` r

# Extract tissue coordinates
coords <- SeuratObject::GetTissueCoordinates(object = seurat_obj, scale = "lowres")
coords <- as.data.frame(coords)
all(c("x", "y") %in% colnames(coords))
# [1] TRUE

# Extract cell metadata
metadata <- seurat_obj[[]]

# Merge coordinates and metadata using common cell identifiers
ids <- intersect(rownames(coords), rownames(metadata))
coords <- cbind(coords[ids, ], metadata[ids, ])

# Construct a GraphSpace object
# Metadata become node attributes
gs <- GraphSpace(coords)

# Add high-dimensional feature data
# Stored separately for lazy aesthetic mapping
gs_fdata(gs) <- SeuratObject::LayerData(seurat_obj, layer = "data")

# If available, add tissue image 
gs_image(gs) <- SeuratObject::GetImage(seurat_obj, mode = "raster")

# Normalize node coordinates to the image space
gs <- normalizeGraphSpace(gs)
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
    #> [1] stxBrain.SeuratData_0.1.2 ssHippo.SeuratData_3.1.4 
    #> [3] pbmc3k.SeuratData_3.1.4   SeuratData_0.2.2.9002    
    #> [5] Seurat_5.5.1.9001         SeuratObject_5.4.0       
    #> [7] sp_2.2-1                  RGraphSpace_1.5.3        
    #> [9] ggplot2_4.0.3            
    #> 
    #> loaded via a namespace (and not attached):
    #>   [1] RColorBrewer_1.1-3     rstudioapi_0.19.0      jsonlite_2.0.0        
    #>   [4] magrittr_2.0.5         spatstat.utils_3.2-3   ggbeeswarm_0.7.3      
    #>   [7] farver_2.1.2           rmarkdown_2.31         fs_2.1.0              
    #>  [10] ragg_1.5.2             vctrs_0.7.3            ROCR_1.0-12           
    #>  [13] spatstat.explore_3.8-1 terra_1.9-34           htmltools_0.5.9       
    #>  [16] sass_0.4.10            sctransform_0.4.3      parallelly_1.47.0     
    #>  [19] KernSmooth_2.23-27     bslib_0.11.0           htmlwidgets_1.6.4     
    #>  [22] desc_1.4.3             ica_1.0-3              fontawesome_0.5.3     
    #>  [25] plyr_1.8.9             plotly_4.12.0          zoo_1.8-15            
    #>  [28] cachem_1.1.0           igraph_2.3.3           mime_0.13             
    #>  [31] lifecycle_1.0.5        pkgconfig_2.0.3        Matrix_1.7-6          
    #>  [34] R6_2.6.1               fastmap_1.2.0          fitdistrplus_1.2-6    
    #>  [37] future_1.70.0          shiny_1.14.0           digest_0.6.39         
    #>  [40] patchwork_1.3.2        tensor_1.5.1           RSpectra_0.16-2       
    #>  [43] irlba_2.3.7            textshaping_1.0.5      progressr_0.19.0      
    #>  [46] spatstat.sparse_3.2-0  httr_1.4.8             polyclip_1.10-7       
    #>  [49] abind_1.4-8            compiler_4.6.1         proxy_0.4-29          
    #>  [52] withr_3.0.3            S7_0.2.2               DBI_1.3.0             
    #>  [55] fastDummies_1.7.6      MASS_7.3-66            rappdirs_0.3.4        
    #>  [58] classInt_0.4-11        tools_4.6.1            units_1.0-1           
    #>  [61] vipor_0.4.7            lmtest_0.9-40          otel_0.2.0            
    #>  [64] beeswarm_0.4.0         httpuv_1.6.17          future.apply_1.20.2   
    #>  [67] goftest_1.2-3          glue_1.8.1             nlme_3.1-170          
    #>  [70] promises_1.5.0         grid_4.6.1             sf_1.1-1              
    #>  [73] Rtsne_0.17             cluster_2.1.8.2        reshape2_1.4.5        
    #>  [76] generics_0.1.4         gtable_0.3.6           spatstat.data_3.1-9   
    #>  [79] class_7.3-24           tidyr_1.3.2            data.table_1.18.4     
    #>  [82] tidygraph_1.3.1        spatstat.geom_3.8-1    RcppAnnoy_0.0.23      
    #>  [85] ggrepel_0.9.8          RANN_2.6.2             pillar_1.11.1         
    #>  [88] stringr_1.6.0          spam_2.11-4            RcppHNSW_0.7.0        
    #>  [91] later_1.4.8            splines_4.6.1          dplyr_1.2.1           
    #>  [94] lattice_0.23-1         deldir_2.0-4           survival_3.8-9        
    #>  [97] tidyselect_1.2.1       miniUI_0.1.2           pbapply_1.7-4         
    #> [100] knitr_1.51             gridExtra_2.3.1        scattermore_1.2       
    #> [103] xfun_0.59              matrixStats_1.5.0      stringi_1.8.9         
    #> [106] lazyeval_0.2.3         yaml_2.3.12            evaluate_1.0.5        
    #> [109] codetools_0.2-20       tibble_3.3.1           cli_3.6.6             
    #> [112] uwot_0.2.4             xtable_1.8-8           reticulate_1.46.0     
    #> [115] systemfonts_1.3.2      jquerylib_0.1.4        dichromat_2.0-1       
    #> [118] Rcpp_1.1.2             spatstat.random_3.5-0  globals_0.19.1        
    #> [121] png_0.1-9              ggrastr_1.0.2          spatstat.univar_3.2-0 
    #> [124] parallel_4.6.1         pkgdown_2.2.0          dotCall64_1.2         
    #> [127] listenv_1.0.0          viridisLite_0.4.3      scales_1.4.0          
    #> [130] e1071_1.7-17           ggridges_0.5.7         crayon_1.5.3          
    #> [133] purrr_1.2.2            rlang_1.3.0            cowplot_1.2.0

## References

Hao, Yuhan, Tim Stuart, Madeline H Kowalski, et al. 2024. “Dictionary
Learning for Integrative, Multimodal and Scalable Single-Cell Analysis.”
*Nature Biotechnology* 42 (2): 293–304.
<https://doi.org/10.1038/s41587-023-01767-y>.
