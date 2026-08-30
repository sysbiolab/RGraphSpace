# Normalize node coordinates to graph and image spaces

Accessory function to normalize node coordinates of a
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object, either by centering nodes within the plot boundaries or by
mapping nodes to pixel coordinates of a background image.

## Usage

``` r
# S4 method for class 'GraphSpace'
normalizeGraphSpace(
  gs,
  mar = 0.1,
  image.space = .has_image(gs),
  flip.x = FALSE,
  flip.y = image.space,
  flip.v = FALSE,
  flip.h = FALSE,
  swap.xy = FALSE,
  equal.mar = FALSE,
  norm.geometry = FALSE,
  verbose = TRUE
)
```

## Arguments

- gs:

  A `GraphSpace` object to be normalized.

- mar:

  A single numeric value in `[0, 0.5]` setting the margins around the
  graph, as a fraction of the final normalized space. For example,
  `mar = 0.1` leaves a margin of 0.1 on each side, so the graph occupies
  the central 0.8 of the space. With an image, the image is cropped to
  the same proportions; if the graph lies close to an image border, the
  crop is shifted or truncated to stay within the image, and the
  requested margin may not be reached.

- image.space:

  Logical; if an image is available, whether to use it as a background
  reference map. When enabled, `x` and `y` graph coordinates are
  interpreted as pixel coordinates in the image matrix. Images can be
  inspected and assigned with
  [`gs_image`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-accessors.md).

- flip.x:

  Logical; whether to flip the node coordinates along the x-axis.

- flip.y:

  Logical; whether to flip the node coordinates along the y-axis. Useful
  for aligning nodes with image backgrounds, which often use an inverted
  coordinate system. Defaults to `image.space`.

- flip.v:

  Logical; whether to vertically flip the background image matrix
  (top-to-bottom) to align with the graph coordinate system.

- flip.h:

  Logical; whether to horizontally flip the background image matrix
  (left-to-right) to align with the graph coordinate system.

- swap.xy:

  Logical; whether to swap x and y node coordinates. Useful when the
  graph coordinate system is transposed relative to the image or
  reference map.

- equal.mar:

  Logical; when an image is available, whether to fit the image with
  equal margins around the graph, resulting in a tighter crop of the
  image. If FALSE (default), the image is fitted to the full square
  figure area, resulting in unequal margins when the graph aspect ratio
  differs from 1. Both methods preserve the aspect ratios of the image
  and graph.

- norm.geometry:

  Logical; when geometries are available, whether to normalize them. If
  `TRUE`,
  [normalizeGeometry](https://sysbiolab.github.io/RGraphSpace/reference/geometry-methods.md)
  is called at the end of the normalization process.

- verbose:

  A single logical value specifying to display detailed messages (when
  `verbose=TRUE`) or not (when `verbose=FALSE`).

## Value

A `GraphSpace` object with updated `nodes` and `image` slots.

## Details

This function re-scales node coordinates to a `[0, 1]` unit square based
on the graph's bounding box when `image.space = FALSE` or, when an image
is provided and `image.space = TRUE`, it maps nodes to pixel
coordinates. It handles image-to-graph alignment via `flip.\*` and
`swap.\*` arguments, used to adjust the graph origin with the image
matrix layout. Users should be aware of the potential discrepancy
between image matrix orientation (top-down) and graph coordinates
(bottom-up). The function attempts to automatically adjust the y-axis to
align the graph's bottom-up coordinates with the image's top-down
layout, but further manual adjustments might be required.

## Note

This is an accessory function typically called during the preprocessing
of `GraphSpace` objects before rendering.

## See also

[`cropGraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-transform.md),
[`gs_image`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-accessors.md)

## Examples

``` r
library(RGraphSpace)
library(igraph)

# Create a star graph
gtoy1 <- make_full_graph(30)

# Create a GraphSpace
gs <- GraphSpace(gtoy1)
#> Validating the 'igraph' object...
#> Vertex attributes 'x' and 'y' missing; computing layout...
#> Vertex attribute 'name' missing; assigning names... 
#> Ignoring graph-level attributes: 'name', 'loops'
#> Creating a 'GraphSpace' object...

gs <- normalizeGraphSpace(gs)
#> Normalizing node coordinates to graph space...

plotGraphSpace(gs, add.labels = TRUE)

```
