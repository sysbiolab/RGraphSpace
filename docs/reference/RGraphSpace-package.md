# RGraphSpace: Rendering Graphs as Coherent Spatial Objects in 'ggplot2'

An interface for rendering 'igraph' objects as 'ggplot2' graphics within
a normalized coordinate space. 'RGraphSpace' implements new geometries
that treat a graph as a single coherent object, synchronizing node and
edge layers under standard aesthetic mappings. Node features are
resolved on demand, supporting high-dimensional data without expanding
node tables. Spatial alignment is available at the pixel level, with
node coordinates anchored to pixel centers through a half-pixel offset,
enabling precise node positioning over external reference frames such as
images and maps. Core functionality builds on 'igraph', 'ggplot2', and
'tidygraph'; optional geometry and large raster-background images use
'sf' and 'terra' when installed.

## Details

For a hands-on introduction, see the vignette:
[`vignette("RGraphSpace")`](https://sysbiolab.github.io/RGraphSpace/articles/RGraphSpace.md).

The full set of documented topics can also be browsed in HTML by running
[`help.start()`](https://rdrr.io/r/utils/help.start.html) and selecting
the RGraphSpace package from the package list.

## References

Sysbiolab Team (2026). *RGraphSpace: Rendering graphs as coherent
spatial objects in ggplot2*. R package version 1.5.4 (Doi:
10.32614/CRAN.package.RGraphSpace),
<https://CRAN.R-project.org/package=RGraphSpace>.

## See also

Useful links:

- <https://github.com/sysbiolab/RGraphSpace>

- <https://sysbiolab.github.io/RGraphSpace/>

- Report bugs at <https://github.com/sysbiolab/RGraphSpace/issues>

## Author

**Maintainer**: Mauro Castro <mauro.a.castro@gmail.com>
([ORCID](https://orcid.org/0000-0003-4942-8131))

Authors:

- Mauro Castro <mauro.a.castro@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-4942-8131))

- Flávio Kessler

- Jonathan Back

- Lana Querne

- Victor Apolonio

Other contributors:

- Vinicius Chagas \[contributor\]
