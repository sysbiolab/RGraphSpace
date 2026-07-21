## *RGraphSpace*: A lightweight interface between *igraph* and *ggplot2* graphics

[![CRAN
status](https://www.r-pkg.org/badges/version/RGraphSpace)](https://cran.r-project.org/package=RGraphSpace)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/RGraphSpace)](https://cran.r-project.org/package=RGraphSpace)
[![License:
Artistic-2.0](https://img.shields.io/badge/license-Artistic--2.0-blue.svg)](https://cran.r-project.org/web/licenses/Artistic-2.0)
[![DOI](https://img.shields.io/badge/doi-10.32614/CRAN.package.RGraphSpace-blue.svg)](https://doi.org/10.32614/CRAN.package.RGraphSpace)

### Highlights

- Native *ggplot2* interface for *igraph* objects
- Coherent rendering of node and edge layers
- Optimized *geoms* for high-dimensional data
- Spatial alignment to external reference frames

### Overview

*RGraphSpace* is an R package that generates *ggplot2* graphics for
*igraph* objects ([Csardi and Nepusz 2006](#ref-nepusz)) within a
normalized coordinate space. The package implements new geometries that
treat a graph as a single coherent object, synchronizing node and edge
layers under standard aesthetic mappings. Node features are resolved on
demand, supporting high-dimensional data without expanding node tables.
Spatial alignment is available at the pixel level, with node coordinates
anchored to pixel centers through a half-pixel offset, enabling precise
node placement over external reference frames such as images and maps.
The package also interoperates with *ggraph* and *tidygraph* workflows
([Pedersen 2024](#ref-pedersen)). Three specialized geoms translate
graph data into geometric layers.

1.  **[`geom_nodespace()`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md)**:
    Renders network nodes. Extends `GeomPoint` aesthetic mappings and
    exposes node state information to the edge layer.

2.  **[`geom_edgespace()`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)**:
    Renders the relationships between nodes. Extends `GeomSegment`
    aesthetic mappings; unlike standard segments, it is node-aware and
    dynamically adjusts start and end points based on node position and
    size.

3.  **[`geom_graphspace()`](https://sysbiolab.github.io/RGraphSpace/reference/geom_graphspace.md)**:
    A convenience wrapper that calls
    [`geom_nodespace()`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md)
    and
    [`geom_edgespace()`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)
    in sequence. Use this for the common case; use the individual geoms
    directly when independent control of node and edge layers is needed.

### Citation

- Sysbiolab Team (2026). RGraphSpace: A lightweight interface between
  ‘igraph’ and ‘ggplot2’ graphics. R package version 1.3.0. Doi:
  10.32614/CRAN.package.RGraphSpace

### Licenses

The *RGraphSpace* package is distributed under
[Artistic-2.0](https://www.r-project.org/Licenses/Artistic-2.0)

### References

- Wickham H: *ggplot2: Elegant Graphics for Data Analysis*.
  Springer-Verlag New York, 2016. <https://ggplot2.tidyverse.org>

- Csardi G, Nepusz T: The igraph software package for complex network
  research. *InterJournal, Complex Systems* 1695. 2006.
  <https://igraph.org>

- Pedersen T: *tidygraph: A Tidy API for Graph Manipulation*. R package
  version 1.3.1. 2024.
  [doi:10.32614/CRAN.package.tidygraph](https://doi.org/10.32614/CRAN.package.tidygraph)
