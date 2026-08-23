# Normalize or fit node geometry

Two related operations for keeping an `sfc` geometry column attached to
a `GraphSpace`'s nodes in registration with the node coordinates, for
two different situations.

## Usage

``` r
# S4 method for class 'GraphSpace'
normalizeGeometry(gs, name = "geometry", verbose = TRUE)

# S4 method for class 'GraphSpace'
fitGeometry(gs, name = "geometry", use_node_size = TRUE, verbose = TRUE)
```

## Arguments

- gs:

  A `GraphSpace` object.

- name:

  Character. Name of the geometry column to operate on.

- verbose:

  Logical. Whether to report progress messages.

- use_node_size:

  Logical. If `TRUE` (the default), `fitGeometry()` also rescales each
  geometry to match its node's `nodeSize`. If `FALSE`, only
  repositioning happens, each feature keeps its current size.

## Value

The updated `GraphSpace` object.

## Details

**`normalizeGeometry`** is for geometry that is already spatially
meaningful, with its own coordinates genuinely correspond to the nodes
(e.g. real cell-segmentation boundaries) and only needs realigning to
the current, normalized node frame. It fits a linear regression between
the geometry's centroids and the node coordinates and rescales the
geometry accordingly, warning if the fit is poor (the geometry did not,
in fact, scale linearly with the nodes).

**`fitGeometry`** is for geometry that is not yet spatially related to
the nodes, as arbitrary shapes used for node markers. It repositions
every shape so its centroid sits exactly at its node's coordinates and,
when `use_node_size = TRUE`, also rescales each shape so its diameter
matches `nodeSize`.

Both require `gs` to already be normalized (see
[`normalizeGraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/normalizeGraphSpace-methods.md)),
and both operate on a single named geometry column, leaving any other
geometry columns untouched.

## Examples

``` r
if (FALSE) { # \dontrun{
# Mode 1: geometry already spatially meaningful, just needs realigning
gs_geometry(gs, "geometry") <- real_cell_boundaries
gs <- normalizeGeometry(gs)

# Mode 2: arbitrary shapes, sized and positioned like nodes
gs_geometry(gs, "geometry") <- arbitrary_shapes
gs <- fitGeometry(gs, use_node_size = TRUE)
} # }
```
