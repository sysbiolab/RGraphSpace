### GraphSpace: A lightweight package for generating ggplot2 graphics for igraph

*GraphSpace* is an R package that integrates *igraph* and *ggplot2* graphics within a spatial map. *GraphSpace* implements new geometric objects using *ggplot2* protypes, customized for representing large *igraph* objects in a normalized coordinate system.

### Installation in R (>=4.2)

##### Install dependencies to build the package's vignettes

```r
install.packages("knitr")
install.packages("rmarkdown")
install.packages("BiocManager")
BiocManager::install("BiocStyle")
```

##### Install the GraphSpace package

```r
install.packages("remotes")
remotes::install_github("sysbiolab/GraphSpace", build_vignettes=TRUE)
```

### Examples

Follow the *GraphSpace* vignette and try to make some *plots*!

```r
library(GraphSpace)
vignette("GraphSpace")
```

### Licenses

The *GraphSpace* package is distributed under [Artistic-2.0](https://www.r-project.org/Licenses/Artistic-2.0)
