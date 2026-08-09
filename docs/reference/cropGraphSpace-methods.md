# Crop node coordinates to graph and image spaces

Accessory function to crop a normalized
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
object.

## Usage

``` r
# S4 method for class 'GraphSpace'
cropGraphSpace(
  gs,
  xmin = 0,
  xmax = 1,
  ymin = 0,
  ymax = 1,
  crop.coord = deprecated()
)
```

## Arguments

- gs:

  A normalized `GraphSpace` object.

- xmin:

  A single number in `[0,1]` specifying the lower x-boundary of the
  plotting area.

- xmax:

  A single number in `[0,1]` specifying the upper x-boundary of the
  plotting area.

- ymin:

  A single number in `[0,1]` specifying the lower y-boundary of the
  plotting area.

- ymax:

  A single number in `[0,1]` specifying the upper y-boundary of the
  plotting area.

- crop.coord:

  Deprecated from RGraphSpace 1.5.1; use cropping boundaries instead.

## Value

A `GraphSpace` object with updated `nodes` and `image` slots.

## Details

This function subsets a normalized graph space to a specific region
defined by the cropping boundaries. It recalculates node positions and
background image boundaries to maintain spatial consistency after
cropping.

## Note

This is an accessory function typically called during the preprocessing
of `GraphSpace` objects before rendering.

## See also

[`normalizeGraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/normalizeGraphSpace-methods.md)

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

gs_crop <- cropGraphSpace(gs, ymax = 0.5)

plotGraphSpace(gs, add.labels = TRUE)


plotGraphSpace(gs_crop, add.labels = TRUE)

```
