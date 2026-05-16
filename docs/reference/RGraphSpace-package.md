# RGraphSpace: A Lightweight Interface Between 'igraph' and 'ggplot2' Graphics

An interface to integrate 'igraph' and 'ggplot2' graphics within a
normalized coordinate system. 'RGraphSpace' implements geometric objects
based on 'ggplot2' prototypes, optimized for the representation of large
networks. The package provides three specialized 'geoms' to translate
graph data into geometric layers, supporting customization of aesthetics
and visual styles. These 'geoms' use a dual-anchor normalization
approach to align layers, required for analyses where network elements
must be referenced to a spatial map. 'RGraphSpace' aims to facilitate
side-by-side visualization of multiple graphs spatially aligned with
reference maps and images.

## Index

|  |  |
|----|----|
| [GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md): | Constructor of GraphSpace-class objects. |
| [plotGraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/plotGraphSpace-methods.md): | Wrapper function to plot GraphSpace objects in ggplot2. |

Further information is available in the vignettes by typing
`vignette('RGraphSpace')`. Documented topics are also available in HTML
by typing [`help.start()`](https://rdrr.io/r/utils/help.start.html) and
selecting the RGraphSpace package from the menu.

## References

Sysbiolab Team (2026). *RGraphSpace: A lightweight interface between
'igraph' and 'ggplot2' graphics*. R package version 1.2.2 (Doi:
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

- Sysbiolab Team

Other contributors:

- Flávio Kessler \[contributor\]

- Jonathan Back \[contributor\]

- Lana Querne \[contributor\]

- Victor Apolonio \[contributor\]

- Vinicius Chagas \[contributor\]
