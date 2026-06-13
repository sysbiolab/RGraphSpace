# RGraphSpace: A Lightweight Interface Between 'igraph' and 'ggplot2' Graphics

An interface to integrate 'igraph' and 'ggplot2' graphics within a
normalized coordinate system. 'RGraphSpace' extends 'ggplot2' with
graph-aware geometries optimized for large networks. The 'GraphSpace'
class integrates directly with 'ggplot2' through specialized 'geoms' and
lazy resolution of node attributes, supporting customization of
aesthetics and visual styles. These 'geoms' use a dual-anchor
normalization approach to align layers, particularly useful for analyses
in which network elements must be spatially aligned with reference maps
and images.

## Index

|  |  |
|----|----|
| [GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md): | Constructor of GraphSpace-class objects. |
| [plotGraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/plotGraphSpace-methods.md): | Wrapper function to plot GraphSpace objects in ggplot2. |

Further information is available in the vignettes by typing
[`vignette('RGraphSpace')`](https://sysbiolab.github.io/RGraphSpace/articles/RGraphSpace.md).
Documented topics are also available in HTML by typing
[`help.start()`](https://rdrr.io/r/utils/help.start.html) and selecting
the RGraphSpace package from the menu.

## References

Sysbiolab Team (2026). *RGraphSpace: A lightweight interface between
'igraph' and 'ggplot2' graphics*. R package version 1.4.0 (Doi:
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
