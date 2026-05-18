### *RGraphSpace*: A lightweight interface between 'igraph' and 'ggplot2' graphics
  <!-- badges: start -->
  [![](https://www.r-pkg.org/badges/version/RGraphSpace)](https://cran.r-project.org/package=RGraphSpace)
  [![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
  [![](https://cranlogs.r-pkg.org/badges/RGraphSpace)](https://cran.r-project.org/package=RGraphSpace)
  [![](https://img.shields.io/badge/license-Artistic--2.0-blue.svg)](https://cran.r-project.org/web/licenses/Artistic-2.0)
  [![](https://img.shields.io/badge/doi-10.32614/CRAN.package.RGraphSpace-blue.svg)](https://doi.org/10.32614/CRAN.package.RGraphSpace)
  <!-- badges: end -->

### Highlights
* Native *ggplot2* interface for *igraph* objects
* Optimized *geoms* for large-scale network visualization
* Aligns networks with reference spatial backgrounds
* Integrated with *ggraph* and *tidygraph* workflows

### Overview

*RGraphSpace* is an R package that generates *ggplot2* graphics for *igraph* objects 
([Csardi and Nepusz 2006](#ref-nepusz)), scaling nodes and edges to a unit space. 
The package implements new *ggplot2* prototypes ([Wickham 2016](#ref-wickham)), 
optimized for representing large networks. This enables extensive customization 
of aesthetics and visual style, including integration with *ggraph* and 
*tidygraph* workflows ([Pedersen 2024](#ref-pedersen)). Three specialized 
`geoms` translate graph data into geometric layers.

1. **`geom_graphspace()`**: A high-level convenience layer that processes both 
nodes and edges in a single call. 
2. **`geom_nodespace()`**: Dedicated to rendering nodes. Inherits `GeomPoint` 
aesthetic mappings, modified to inform the edge layer on node states. 
3. **`geom_edgespace()`**: Handles the relational data between nodes. Inherits 
`GeomSegment` aesthetic mappings; unlike standard segments, it is "node-aware" 
and dynamically calibrates start and end points to connected nodes. 

### Installation in R (>=4.5)

##### Dependencies to build the vignettes
```r
install.packages("knitr")
install.packages("rmarkdown")
install.packages("remotes")
```
##### Development version
```r
remotes::install_github("sysbiolab/RGraphSpace", build_vignettes=TRUE)
```

### Tutorials

* https://sysbiolab.github.io/RGraphSpace

* https://sysbiolab.github.io/PathwaySpace

### Citation

If you use *RGraphSpace*, please cite:

* Sysbiolab Team (2026). RGraphSpace: A lightweight interface between 'igraph' and 'ggplot2' graphics. R package version 1.2.0. Doi: 10.32614/CRAN.package.RGraphSpace

### Licenses

The *RGraphSpace* package is distributed under [Artistic-2.0](https://www.r-project.org/Licenses/Artistic-2.0)

### References

<a name="ref-wickham"></a>

* Wickham H: *ggplot2: Elegant Graphics for Data Analysis*. Springer-Verlag New York, 2016. [https://ggplot2.tidyverse.org](https://ggplot2.tidyverse.org)

<a name="ref-nepusz"></a>

* Csardi G, Nepusz T: The igraph software package for complex network research. *InterJournal, Complex Systems* 1695. 2006. [https://igraph.org](https://igraph.org)

<a name="ref-pedersen"></a>

* Pedersen T: *tidygraph: A Tidy API for Graph Manipulation*. R package version 1.3.1. 2024. [doi:10.32614/CRAN.package.tidygraph](https://doi.org/10.32614/CRAN.package.tidygraph)
