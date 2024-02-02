### RGraphSpace: A lightweight package for representing large igraph objects in a normalized coordinate system.

*RGraphSpace* is an R package that integrates *igraph* and *ggplot2* graphics within spatial maps. *RGraphSpace* implements new geometric objects using *ggplot2* prototypes, customized for representing large *igraph* objects in a normalized coordinate system. By scaling shapes and graph elements, *RGraphSpace* can provide a framework for layered visualizations.

### Installation in R (>=4.3)

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

### Licenses

The *RGraphSpace* package is distributed under [Artistic-2.0](https://www.r-project.org/Licenses/Artistic-2.0)
