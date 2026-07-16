---
title: 'RGraphSpace: A lightweight interface between igraph and ggplot2 graphics'
tags:
  - R
  - graph visualization
  - network analysis
  - high-dimensional data
  - spatial anchors
authors:
  - name: Flávio Gabriel Carazza-Kessler
    orcid: 0000-0002-5309-8043
    equal-contrib: true
    affiliation: 1
  - name: Jonathan André Back
    orcid: 0009-0008-7338-1197
    equal-contrib: true
    affiliation: 1
  - name: Lana Bazan Peters Querne
    orcid: 0000-0001-9967-028X
    equal-contrib: true
    affiliation: 1
  - name: Victor Henrique Apolonio dos Santos
    orcid: 0000-0002-6394-5840
    affiliation: 1
  - name: Mauro Antonio Alves Castro
    orcid: 0000-0003-4942-8131
    corresponding: true
    affiliation: 1
affiliations:
 - name: Bioinformatics and Systems Biology Laboratory, Federal University of Paraná, Curitiba, PR, 81520-260, Brazil
   index: 1
   ror: 05syd6y78
date: 13 July 2026
bibliography: paper.bib
---

# Summary

Network visualization is a fundamental component of computational biology and systems science research. While R offers powerful tools for network analysis through `igraph`and sophisticated plotting capabilities through `ggplot2`, integrating these frameworks for spatial network visualization has remained challenging. `RGraphSpace` addresses this gap by providing a lightweight interface that seamlessly integrates `igraph` objects with `ggplot2` graphics within a normalized coordinate system. The package implements new geometric objects using `ggplot2` prototypes, specifically customized for side-by-side visualization of multiple graphs. By scaling shapes and graph elements to fit within a standardized unit space, `RGraphSpace` enables layered visualizations with consistent spatial alignment, making it particularly valuable for comparative network analysis and spatial mapping applications.