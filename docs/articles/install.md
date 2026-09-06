# Installation Instructions

To install *RGraphSpace*, R version [4.5](https://www.r-project.org/) or
greater is required.

## Release version

``` r

# Release version from CRAN
install.packages("RGraphSpace")
```

## Development version

``` r

# Dependencies to build the vignettes
install.packages("knitr")
install.packages("rmarkdown")
install.packages("remotes")

# Package source
remotes::install_github("sysbiolab/RGraphSpace", build_vignettes=TRUE)
```

#### Other packages used in the tutorials

``` r

# Used in the introductory vignettes
install.packages("ggnewscale")
install.packages("patchwork")

# Used in the general-purpose vignettes
install.packages("ggraph")
install.packages("dplyr")
install.packages("sf")
install.packages("geometry")
install.packages("maps")
install.packages("flightsbr")
install.packages("airportr")
install.packages("BiocManager")
BiocManager::install("RedeR")

# Used in the advanced vignettes
install.packages("sf")
install.packages("terra")
install.packages("Seurat")
BiocManager::install("glmGamPoi")
BiocManager::install("SpatialExperiment")
BiocManager::install("SpatialFeatureExperiment")
BiocManager::install("VisiumIO")
BiocManager::install("OSTA.data")
remotes::install_github("satijalab/seurat-data")
```
