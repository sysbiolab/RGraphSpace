---
title: 'RGraphSpace: Rendering graphs as coherent spatial objects in ggplot2'
tags:
- R
- graph visualization
- network analysis
- high-dimensional data
- spatial anchors
date: "16 July 2026"
output:
  pdf_document: default
  html_document:
    df_print: paged
authors:
- name: Flávio Gabriel Carazza-Kessler
  orcid: "0000-0002-5309-8043"
  equal-contrib: true
  affiliation: 1
- name: Jonathan André Back
  orcid: "0009-0008-7338-1197"
  equal-contrib: true
  affiliation: 1
- name: Lana Bazan Peters Querne
  orcid: "0000-0001-9967-028X"
  equal-contrib: true
  affiliation: 1
- name: Victor Henrique Apolonio dos Santos
  orcid: "0000-0002-6394-5840"
  affiliation: 1
- name: Mauro Antonio Alves Castro
  orcid: "0000-0003-4942-8131"
  corresponding: true
  affiliation: 1
bibliography: paper.bib
affiliations:
- name: "Bioinformatics and Systems Biology Laboratory, Federal University of Paraná,
    Curitiba, PR, 81520-260, Brazil"
  index: 1
  ror: 05syd6y78
---

# Summary

A graph describes a network structure of nodes connected by edges, and is used across many areas of science to represent relationships between entities. When the relationships themselves are spatial, node positions carry physical meaning, and the graph must be further represented within an external frame of reference. In R, *igraph* [@Nepusz2006] provides powerful tools for network analysis, and the *ggplot2* ecosystem [@Wickham2016] for visualization, drawing nodes and edges as separate layers. Integrating the two remains challenging, however, because the geometry of these layers is not fully coordinated at rendering time, a consequence of *ggplot2*'s input model, in which each plot layer consumes a single rectangular table and is resolved independently. This design works remarkably well for most applications but is restrictive for graph structures with mutually dependent components. Because layers do not share transformed state, the graph is not rendered as a single coherent object. Here we introduce *RGraphSpace*, a lightweight interface that integrates *igraph* with *ggplot2* graphics within a normalized coordinate space. It synchronizes node and edge layers, jointly resolving their geometry under standard aesthetic mappings. This coherence lets the graph be anchored to external reference frames, where it becomes a spatial object embedded in a wider context. *RGraphSpace* is available from CRAN, with comprehensive tutorials provided on its documentation site (<https://sysbiolab.github.io/RGraphSpace>).

# Statement of need

Graph handling in R rests on two mature foundations: *igraph* [@Nepusz2006] for network analysis and *ggplot2* [@Wickham2016] for layered visualization. The *ggraph* package [@Pedersen2025_2] bridges these, exposing relational data to the grammar of graphics. Graph data manipulation has similarly matured through *tidygraph* [@Pedersen2025_1], which represents a graph as tidy node and edge tables.

Yet rendering a graph as a spatial object requires coordination across its components. Because *ggplot2* resolves each layer independently, node and edge layers are not fully synchronized. Positional coordinates are shared and remain consistent, but other aesthetics are resolved per layer, so edges have no access to the rendered sizes of the nodes.

*RGraphSpace* addresses this by mapping an `igraph` object into a normalized coordinate space in which nodes, edges, and their associated elements are resolved together. With node and edge geometries synchronized at rendering time, edges are drawn with respect to the final extent of the nodes they connect, and this correspondence is maintained even when node attributes are rescaled through the *ggplot2* grammar.

This normalized space serves a second purpose, placing the graph within a broader spatial context, aligned to external reference frames such as microscopy images. Graph-image alignment requires consistent coordinate conventions; a mismatch in axis orientation, for example a top-left versus bottom-left origin, leaves the nodes reflected relative to the image (\autoref{fig:figure1}A). Moreover, because each pixel cell occupies a finite square area, a half-pixel offset is needed to prevent node positions from drifting toward the borders (\autoref{fig:figure1}B). This discrepancy may seem negligible in most cases but can matter when nodes represent measurements anchored to specific spatial features. Reconciling these conventions lets the graph be rendered as a coherent spatial object rather than an isolated diagram.

![Spatial alignment. (**A**) Aligning a toy graph to a background image. Left: node coordinates are assumed to be pixel indices of the reference image, but rendering both in *ggplot2* requires a shared, normalized coordinate space. Top right: with `y-flip`, the two are rendered in correspondence. Bottom right: without it, the graph reflects relative to the image. The red node breaks symmetry and highlights orientation. (**B**) Two normalization schemes, where `i` denotes a pixel or node index along the axis. Top: *anchors at pixel centers* use $(i-\tfrac{1}{2})/n$, introducing a half-pixel offset. Bottom: *anchors at image borders* use $(i-1)/(n-1)$, mapping positions across the full normalized interval $[0,1]$. The two coincide mid-axis but drift apart toward the ends. *RGraphSpace* applies the `y-flip` and pixel-center anchoring by default.\label{fig:figure1}](figure1.png){width="100%"}

# State of the field

The Grammar of Graphics [@Wilkinson2005] constructs statistical graphics by decomposing a plot into independent, composable elements: data, aesthetic mappings, geometric objects, scales, coordinate systems, and statistical transformations. A graphic is then built declaratively by combining these components. In R, the *ggplot2* package implements a layered form of this grammar [@Wickham2010], assembling graphics by adding successive layers to a shared coordinate system, and has become the dominant visualization framework in the R ecosystem.

Central to this design is how *ggplot2* consumes data: each layer is bound to a single rectangular table and resolved independently, applying its own transformations, scales, and position adjustments before being drawn [@Wickham2016]. This independence is a deliberate strength, since layers can be freely combined, reordered, and reused, and it pairs naturally with the tidy data convention [@Wickham2014], in which each variable is a column and each observation a row.

This design has allowed *ggplot2* to accommodate data structures beyond simple flat tables. The *sf* package [@Pebesma2018], for example, represents spatial vector data as list-columns and integrates with *ggplot2* through a dedicated `geom_sf` layer, extending the grammar to handle spatial geometry while preserving its declarative interface. Graph data has received similar attention. The *ggraph* package [@Pedersen2025_2] extends *ggplot2* with an extensive family of node and edge geometries, bringing graph layouts into the grammar, while other tools such as *ggnetwork* [@Briatte2026] and *GGally* [@Schloerke2025] provide comparable functionality for plotting network data. These packages establish that the node and edge tables of a graph can be expressed through aesthetic mappings and rendered as separate *ggplot2* layers. They do not, however, synchronize node and edge geometry during rendering.

Alongside these visualization tools, a parallel line of work has focused on making complex data structures accessible to the tidyverse [@Wickham2019], a collection of R packages sharing a common design for data manipulation. The *tidygraph* package [@Pedersen2025_1] represents a graph as a pair of tidy node and edge tables, allowing graph manipulation through familiar tidyverse verbs while preserving its relational structure. This tidy philosophy extends to multi-component containers common in computational biology [@Hutchison2024], including *Seurat* [@Hao2024] and *SpatialExperiment* [@Righelli2022], which bundle components such as assays, metadata, reductions, spatial coordinates, and graphs within a single object. *RGraphSpace* builds on this well-supported foundation, integrating with *tidygraph* to contribute the rendering step.

A related task arises in the context of multi-component containers, when graphs derived from spatially resolved assays are displayed alongside reference images: graph and image coordinate conventions differ and must be explicitly reconciled for meaningful overlay. This task is adjacent to image registration, a broad field concerned with estimating a spatial transformation that aligns a query to a reference dataset [@Lewis2021; @Balluff2022]. *RGraphSpace* addresses a special case where the spatial correspondence between graph and image is known by construction, so the alignment reduces to correcting axis-orientation mismatches, fixing node positions to their target pixels, and normalizing graph and image to a common coordinate space.

# Research impact statement

*RGraphSpace* provides the spatial foundation for the *PathwaySpace* package [@PathwaySpace], which projects network-derived signals onto the normalized coordinate space, transforming discrete vertex signals into continuous surfaces over the graph topology. This integration has supported published analyses in systems biology [@Tercan2025; @Ellrott2025], demonstrating *RGraphSpace*'s utility for downstream spatial analysis tools.

# Software design

*RGraphSpace* is built around the `GraphSpace` S4 class, which encapsulates a graph together with the components required for coherent rendering: node and edge tables, feature data, source and render-ready images, and metadata (\autoref{fig:figure2}A). Object validity enforces a shared node identity across the graph vertices, node table, and feature rows. Rather than embedding node-associated features as node attributes, `GraphSpace` stores them in `@fdata`, a dedicated sparse-matrix slot aligned to nodes but structurally independent of the node table. When a feature is mapped to an aesthetic, only the requested feature is retrieved and joined for the current plot, so graphs carrying thousands of features are never expanded into dense node tables.

Graphs can be supplied as either `igraph` or `tidygraph` objects through a common interface (\autoref{fig:figure2}B). *RGraphSpace* also works with *ggraph*, accepting its layouts as input and providing geometries that can be used within *ggraph* plots. Coercion methods extend the same interface to selected multi-component containers, loading their node-associated features into the `@fdata` slot.

The `normalizeGraphSpace()` function scales node coordinates into a unit square aligned to a reference frame. By default, this frame is the graph's own extent, centered within the square; when a background image is available, the image space is used instead. In this case, node coordinates are assumed to be pixel indices of the image matrix. The orientation and pixel-anchoring corrections are depicted in \autoref{fig:figure1}. The `normalizeGraphSpace()` function also exposes arguments for graph-image alignment operations, demonstrated with code in the online tutorials.

A `GraphSpace` supplied to `ggplot()` produces a subclassed plot that verifies, through a shared `@uuid`, that the node and edge layers originate from the same graph before coordinating them. *RGraphSpace* then intercepts the build pipeline after the node layer has been fully processed, and the resulting rendered sizes are passed to the edge layer, allowing edge construction to use the final node representation. This synchronization is available through three geometries: `geom_nodespace()`, `geom_edgespace()`, and the convenience wrapper `geom_graphspace()`.

![Architecture of *RGraphSpace*. (**A**) The `GraphSpace` S4 class stores a graph, its node and edge tables, feature data, source and render-ready images, and metadata. Node identity is enforced by `setValidity`. (**B**) Graph inputs (`igraph` or `tidygraph`) are used to construct a `GraphSpace`; selected non-graph objects are handled through coercion and accessors (inset). Coordinates are normalized to a unit square, centered on the graph's own extent or aligned to a background image. At the *ggplot2* build step, the node and edge layers are synchronized and rendered over an optional background image.\label{fig:figure2}](figure2.png){width="100%"}

# Availability and documentation

*RGraphSpace* is available from CRAN (<https://cran.r-project.org/package=RGraphSpace>) and the development version is hosted on GitHub (<https://github.com/sysbiolab/RGraphSpace>). Documentation is available at <https://sysbiolab.github.io/RGraphSpace> and includes comprehensive tutorials covering the full workflow, illustrated here by graph construction from an `igraph` object (\autoref{fig:figure3}A), a graph rendered as a spatial object over a background image (\autoref{fig:figure3}B), and application to spatial feature data (\autoref{fig:figure3}C).

![Selected tutorials available at <https://sysbiolab.github.io/RGraphSpace>. (**A**) Graph construction: a graph rendered from an `igraph` object using *RGraphSpace* geometries. (**B**) Image alignment: a graph rendered as a spatial object over a background image. (**C**) Spatial feature mapping: graph and feature data rendered together, illustrated with spatial transcriptomics data.\label{fig:figure3}](figure3.png){width="100%"}

# AI usage disclosure

During the preparation of this work, the authors used ChatGPT (OpenAI) and Claude Code (Anthropic) to improve text readability and to audit code while using RStudio Desktop (<https://posit.co/>). The authors carefully reviewed and edited the content as needed after using these tools and assume full responsibility for the published content.

# Acknowledgements

This work was funded by CNPq (440412/2022-6 and 307144/2025-9), CAPES (Finance Code 001), and Fundação Araucária (NAPI Bioinformática).

# References
