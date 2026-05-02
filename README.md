## *RGraphSpace*: A lightweight interface between 'igraph' and 'ggplot2' graphics
  <!-- badges: start -->
  [![](https://www.r-pkg.org/badges/version/RGraphSpace)](https://cran.r-project.org/package=RGraphSpace)
  [![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
  [![](https://cranlogs.r-pkg.org/badges/RGraphSpace)](https://cran.r-project.org/package=RGraphSpace)
  [![](https://img.shields.io/badge/license-Artistic--2.0-blue.svg)](https://cran.r-project.org/web/licenses/Artistic-2.0)
  [![](https://img.shields.io/badge/doi-10.32614/CRAN.package.RGraphSpace-blue.svg)](https://doi.org/10.32614/CRAN.package.RGraphSpace)
  <!-- badges: end -->
  
*RGraphSpace* is an R package that generates *ggplot2* graphics for *igraph* 
objects, scaling nodes and edges to a unit space. The package implements new
*ggplot2* prototypes, optimized for representing large networks. This enables 
extensive customization of aesthetics and visual style, including colors, 
shapes, and line types. Three specialized `geoms` translate graph data into 
geometric layers. These `geoms` use a dual-anchor normalization approach 
to align layers, required for analysis where network elements must be 
accurately referenced to a spatial map. 

1. **`geom_graphspace()`**: A high-level convenience layer that processes both 
nodes and edges in a single call. 
2. **`geom_nodespace()`**: Dedicated to rendering nodes. Inherits `GeomPoint` 
aesthetic mappings, modified to inform the edge layer on node states. 
3. **`geom_edgespace()`**: Handles the relational data between nodes. Inherits 
`GeomSegment` aesthetic mappings; unlike standard segments, it is "node-aware" 
and dynamically calibrates start and end points to connected nodes. 

By scaling graph elements, *RGraphSpace* also supports the overlay of networks 
onto image features. Its dual-anchor normalization approach aligns nodes with 
matrix indices, which is critical for pixel-level precision. 

*RGraphSpace* may provide infrastructure support for packages that require 
graph data abstraction. For example, it currently provides classes and 
methods inherited by [PathwaySpace](https://github.com/sysbiolab/PathwaySpace), 
a tool for exploring signal patterns in spatial transcriptomics. 

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

* Sysbiolab Team (2026). RGraphSpace: A lightweight interface between 'igraph' and 'ggplot2' graphics. R package version 1.2.0. Doi: 10.32614/CRAN.package.RGraphSpace

### Licenses

The *RGraphSpace* package is distributed under [Artistic-2.0](https://www.r-project.org/Licenses/Artistic-2.0)
