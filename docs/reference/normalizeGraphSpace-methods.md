# Normalize node coordinates to graph and image spaces

Accessory functions to normalize node coordinates in GraphSpace, either
by centering them within the graph boundaries or by mapping them to
pixel coordinates of a background image.

## Usage

``` r
# S4 method for class 'GraphSpace'
normalizeGraphSpace(
  gs,
  mar = 0.1,
  use_image = FALSE,
  flip.x = FALSE,
  flip.y = FALSE,
  rotate.xy = FALSE,
  flip.v = FALSE,
  flip.h = FALSE,
  verbose = TRUE,
  image = deprecated()
)

# S4 method for class 'GraphSpace'
cropGraphSpace(gs, crop.coord = c(0, 1, 0, 1), verbose = TRUE)
```

## Arguments

- gs:

  A `GraphSpace` object to be normalized.

- mar:

  A single numeric value in `[0, 0.5]` controlling the size of the outer
  margins around the graph. Without an image, `mar` specifies symmetric
  margins as a fraction of the graph space. With an image, `mar` is
  interpreted as a fraction of the available image margins surrounding
  the graph.

- use_image:

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
  coordinate system.

- rotate.xy:

  Logical; whether to rotate x-y coordinates.

- flip.v:

  Logical; whether to vertically flip the background image matrix
  (top-to-bottom) to align with the graph coordinate system.

- flip.h:

  Logical; whether to horizontally flip the background image matrix
  (left-to-right) to align with the graph coordinate system.

- verbose:

  A single logical value specifying to display detailed messages (when
  `verbose=TRUE`) or not (when `verbose=FALSE`).

- image:

  Deprecated from RGraphSpace 1.3.0; use
  [gs_image](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-accessors.md)
  instead.

- crop.coord:

  An optional numeric vector of length four specifying a cropping region
  (xmin, xmax, ymin, ymax), with values in normalized coordinates
  `[0,1]`.

## Value

A `GraphSpace` object with updated `nodes` and `image` slots.

## Details

These functions provide different strategies for coordinate
transformation:

- **normalizeGraphSpace**: Re-scales node coordinates to a `[0, 1]` unit
  square based on the graph's bounding box (when `use_image = FALSE`) or
  maps them to pixel coordinates (when `use_image = TRUE` and an image
  is provided; see
  [gs_image](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-accessors.md)).
  It handles image-to-graph alignment via `flip.\*` and `rotate.\*`
  arguments, used to adjust the graph origin with the image matrix
  layout. Users should be aware of the potential discrepancy between
  image matrix orientation (top-down) and graph coordinates (bottom-up).
  The function attempts to automatically adjust the y-axis to align the
  graph's bottom-up coordinates with the image's top-down layout, but
  further manual adjustments might be required.

- **cropGraphSpace**: Subsets the normalized graph space into a specific
  region defined by `crop.coord`. It recalculates node positions and
  background image boundaries to maintain spatial consistency after
  cropping. This function requires a previously normalized `GraphSpace`
  object.

## Note

This is an accessory function typically called during the preprocessing
of `GraphSpace` objects before rendering.

## See also

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
#> Mutual edges detected: Simplified for data frame...
#> Arrows recoded to bidirectional display
#> Edge attributes retained from the first occurrence
#> Creating a 'GraphSpace' object...

gs <- normalizeGraphSpace(gs)
#> Normalizing node coordinates to graph space...

gs_crop <- cropGraphSpace(gs, 
         crop.coord = c(0, 0.75, 0, 0.75))
```
