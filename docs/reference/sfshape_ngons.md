# Build regular polygons

Construct one or more regular polygons (equal sides and angles) as
`sfg`/`sfc` `POLYGON` geometry. `sfshape_ngon()` builds a single polygon
at a given center; `sfshape_ngons()` builds `n` polygons, automatically
arranged in a compact, non-overlapping grid. Useful for building varied,
non-hand-typed node geometries; see the geometry vignette for examples,
and
[`sfshape_stars`](https://sysbiolab.github.io/RGraphSpace/reference/sfshape_stars.md)
for the star-shaped equivalent.

## Usage

``` r
sfshape_ngon(cx = 0, cy = 0, sides = 5, radius = 1)

sfshape_ngons(n, sides = c(3, 5, 7), radius = 0.3, spacing = NULL)
```

## Arguments

- cx, cy:

  Numeric. Coordinates of the polygon's center. (`sfshape_ngon()` only.)

- sides:

  Integer. Number of sides; must be 3 or more. For `sfshape_ngons()`,
  may be a vector, recycled across the `n` polygons to vary shape per
  polygon.

- radius:

  Numeric. Circumradius – distance from the center to each vertex. For
  `sfshape_ngons()`, may be a vector, recycled across the `n` polygons.

- n:

  Integer. Number of polygons to build. (`sfshape_ngons()` only.)

- spacing:

  Numeric. Distance between polygon centers in the auto-generated grid.
  Defaults to `max(radius) * 2.5`, which guarantees no overlap.
  (`sfshape_ngons()` only.)

## Value

`sfshape_ngon()` returns a single `sfg` object of type `POLYGON`.
`sfshape_ngons()` returns an `sfc` of `n` such polygons.

## See also

[`sfshape_stars`](https://sysbiolab.github.io/RGraphSpace/reference/sfshape_stars.md)

## Examples

``` r
if (requireNamespace("sf", quietly = TRUE)){
  pentagon <- sfshape_ngon(0, 0, sides = 5, radius = 1)
  hexagon  <- sfshape_ngon(2, 0, sides = 6, radius = 1)
  plot(sf::st_sfc(pentagon, hexagon))
  many <- sfshape_ngons(7, sides = c(3, 5, 8), radius = 0.4)
  plot(many)
}


```
