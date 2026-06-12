# Annotate a GraphSpace Plot

`annotation_gspace()` is a generic dispatcher for adding annotation
layers to a `ggplot`-based `GraphSpace` plot. The `type` argument
selects the annotation type; additional arguments are forwarded to the
corresponding handler.

## Usage

``` r
annotation_gspace(..., type = "image")

annotation_gspace_image(
  raster,
  interpolate = FALSE,
  opacity = 1,
  flip.v = FALSE,
  flip.h = FALSE
)
```

## Arguments

- ...:

  Arguments forwarded to the annotation handler selected by `type`.

- type:

  A string specifying the annotation type. Currently available:
  `"image"` (default), dispatched to `annotation_gspace_image()`.

- raster:

  An image to be displayed. Accepted types:

  - A
    [`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
    object — the image is extracted via
    [`gs_image`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-accessors.md).

  - A `raster` object (see
    [`as.raster`](https://rdrr.io/r/grDevices/as.raster.html)).

  - A `matrix` or 3D array (RGB/RGBA), coerced to `raster`
    automatically.

- interpolate:

  A logical value indicating whether to apply linear interpolation when
  the image is rendered at a different resolution than its native size.
  Defaults to `FALSE`.

- opacity:

  A numeric value in `[0, 1]` controlling the transparency of the image.
  `1` is fully opaque (default); `0` is fully transparent.

- flip.v:

  A logical value; if `TRUE`, the image is flipped vertically
  (top-to-bottom). Defaults to `FALSE`.

- flip.h:

  A logical value; if `TRUE`, the image is flipped horizontally
  (left-to-right). Defaults to `FALSE`.

## Value

A ggplot2 layer object that can be added to a
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html) call
with `+`, or `invisible(NULL)` with a warning if the image could not be
resolved.

## See also

[`annotation_raster`](https://ggplot2.tidyverse.org/reference/annotation_raster.html),
[`gs_image`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-accessors.md),
[`geom_nodespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md),
[`geom_edgespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md)

## Examples

``` r

# Assuming 'gs' is a GraphSpace object with 
# an image stored in gs_image(gs)

if (FALSE) { # \dontrun{
# Pass a GraphSpace object directly
ggplot(gs) +
  annotation_gspace(gs) +
  geom_edgespace() +
  geom_nodespace()

# Extract the image explicitly
ggplot(gs) +
  annotation_gspace(gs_image(gs)) +
  geom_edgespace() +
  geom_nodespace()

# Dim the background and flip vertically
ggplot(gs) +
  annotation_gspace(gs, opacity = 0.5, flip.v = TRUE) +
  geom_edgespace() +
  geom_nodespace()
  
} # }
```
