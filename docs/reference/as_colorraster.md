# Map numeric values to a color raster

Helper function that converts numeric values to colors and returns a
raster image. Useful for visualizing numeric matrices as color
backgrounds.

## Usage

``` r
as_colorraster(x, palette = hcl.colors(30), na.color = "white")
```

## Arguments

- x:

  A numeric vector or matrix containing values to be mapped to colors.

- palette:

  A vector of colors used as the palette. By default, `hcl.colors(30)`
  is used.

- na.color:

  Color used for `NA` values. Defaults to `NA`.

## Value

A raster object as produced by
[`as.raster()`](https://rdrr.io/r/grDevices/as.raster.html).

## Details

Values in `x` are rescaled to the range of the palette using
[`scales::rescale()`](https://scales.r-lib.org/reference/rescale.html),
and each value is mapped to a corresponding color. If `x` is a matrix,
the resulting raster preserves the same dimensions.

## Examples

``` r
library(RGraphSpace)

# Convert the volcano matrix to a color raster
img <- as_colorraster(volcano)
plot(img)

```
