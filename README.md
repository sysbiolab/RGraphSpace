### *RGraphSpace*: A lightweight interface between 'ggplot2' and 'igraph' objects.
  <!-- badges: start -->
  [![](https://www.r-pkg.org/badges/version/RGraphSpace)](https://cran.r-project.org/package=RGraphSpace)
  [![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
  [![](https://cranlogs.r-pkg.org/badges/RGraphSpace)](https://cran.r-project.org/package=RGraphSpace)
  [![](https://img.shields.io/badge/license-Artistic--2.0-blue.svg)](https://cran.r-project.org/web/licenses/Artistic-2.0)
  [![](https://img.shields.io/badge/doi-10.32614/CRAN.package.RGraphSpace-blue.svg)](https://doi.org/10.32614/CRAN.package.RGraphSpace)
  <!-- badges: end -->
*RGraphSpace* is an *R* package that integrates *igraph* and *ggplot2* graphics within spatial maps. *RGraphSpace* implements new geometric objects using *ggplot2* prototypes, customized for representing *igraph* objects in a normalized coordinate system. By scaling shapes and graph elements, *RGraphSpace* can provide a framework for layered visualizations.

### Installation in R (>=4.4)

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
