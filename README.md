### *RGraphSpace*: A lightweight interface between 'ggplot2' and 'igraph' objects.
  <!-- badges: start -->
  [![](https://www.r-pkg.org/badges/version/RGraphSpace)](https://cran.r-project.org/package=RGraphSpace)
  [![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
  [![](https://cranlogs.r-pkg.org/badges/RGraphSpace)](https://cran.r-project.org/package=RGraphSpace)
  [![](https://img.shields.io/badge/license-Artistic--2.0-blue.svg)](https://cran.r-project.org/web/licenses/Artistic-2.0)
  [![](https://img.shields.io/badge/doi-10.32614/CRAN.package.RGraphSpace-blue.svg)](https://doi.org/10.32614/CRAN.package.RGraphSpace)
  <!-- badges: end -->
  
*RGraphSpace* is an R package that generates *ggplot2* graphics for *igraph* 
objects, scaling nodes and edges to a standardized unit space. This facilitates 
the side-by-side visualization of multiple graphs. The package implements new 
geometric objects based on *ggplot2* prototypes, optimized for representing 
large networks. The integration with *ggplot2* enables customization of aesthetics 
and visual style, including colors, shapes, and line types. Three `geoms` are 
used to translate graph data to *ggplot2* graphics: 

1. **`geom_graphspace()`**: A high-level convenience layer that process both 
node and edge layers in a single call.
2. **`geom_nodespace()`**: Dedicated to rendering vertices, optimized to ensure 
that nodes and edges are correctly scaled between layers.
3. **`geom_edgespace()`**: Handles the relational data between nodes. Unlike 
standard segment geoms, it is "node-aware", meaning it can dynamically adjust 
edges based on connected nodes.

By scaling graph elements, *RGraphSpace* supports the overlay of networks onto 
image features. It uses a dual-anchor normalization approach to align nodes 
with matrix indices, which is critical for layered analysis where graph elements 
must be accurately referenced to a spatial map with pixel-level precision. 

*RGraphSpace* gives infrastructure support for the 
[PathwaySpace](https://github.com/sysbiolab/PathwaySpace) package for
exploring signal patterns in spatial transcriptomics.

### Installation in R (>=4.5)

##### Install dependencies to build the package's vignettes

```r
install.packages("knitr")
install.packages("rmarkdown")
```

##### Install the RGraphSpace package

```r
install.packages("remotes")
remotes::install_github("sysbiolab/RGraphSpace", build_vignettes=TRUE)
```

### Examples

Follow the *RGraphSpace* vignette and try to make some *plots*!

```r
library(RGraphSpace)
vignette("RGraphSpace")
```

### Citation

If you use *RGraphSpace*, please cite:

* Sysbiolab Team. "*RGraphSpace*: A lightweight package for representing large igraph objects in a normalized coordinate system". R package, 2023. Doi: 10.32614/CRAN.package.RGraphSpace

* Castro MAA, Wang X, Fletcher MNC, Meyer KB, Markowetz F. "*RedeR*: R/Bioconductor package for representing modular structures nested networks and multiple levels of hierarchical associations". *Genome Biology* 13:R29, 2012. Doi: 10.1186/gb-2012-13-4-r29

### Licenses

The *RGraphSpace* package is distributed under [Artistic-2.0](https://www.r-project.org/Licenses/Artistic-2.0)
