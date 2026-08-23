# Build star polygons

Construct one or more star-shaped polygons, alternating between an outer
and an inner radius at each vertex, as `sfg`/`sfc` `POLYGON` geometry.
`sfshape_star()` builds a single star at a given center;
`sfshape_stars()` builds `n` stars, automatically arranged in a compact,
non-overlapping grid. Useful for building varied, non-hand-typed node
geometries; see the geometry vignette for examples, and
[`sfshape_ngons`](https://sysbiolab.github.io/RGraphSpace/reference/sfshape_ngons.md)
for the regular-polygon equivalent.

## Usage

``` r
sfshape_star(cx = 0, cy = 0, points = 5, r_outer = 0.3, r_inner = 0.1)

sfshape_stars(
  n,
  points = c(3, 4, 5),
  r_outer = 0.3,
  r_inner = 0.1,
  spacing = NULL
)
```

## Arguments

- cx, cy:

  Numeric. Coordinates of the star's center. (`sfshape_star()` only.)

- points:

  Integer. Number of star points; must be 2 or more. For
  `sfshape_stars()`, may be a vector, recycled across the `n` stars to
  vary shape per star.

- r_outer:

  Numeric. Radius to each outer (point) vertex. For `sfshape_stars()`,
  may be a vector, recycled across the `n` stars.

- r_inner:

  Numeric. Radius to each inner (valley) vertex. Smaller values relative
  to `r_outer` produce sharper points; values closer to `r_outer`
  produce a rounder, less pronounced star. For `sfshape_stars()`, may be
  a vector, recycled across the `n` stars.

- n:

  Integer. Number of stars to build. (`sfshape_stars()` only.)

- spacing:

  Numeric. Distance between star centers in the auto-generated grid.
  Defaults to `max(r_outer) * 2.5`, which guarantees no overlap.
  (`sfshape_stars()` only.)

## Value

`sfshape_star()` returns a single `sfg` object of type `POLYGON`.
`sfshape_stars()` returns an `sfc` of `n` such stars.

## See also

[`sfshape_ngons`](https://sysbiolab.github.io/RGraphSpace/reference/sfshape_ngons.md)

## Examples

``` r
star5 <- sfshape_star(0, 0, points = 5, r_outer = 1, r_inner = 0.4)
star8 <- sfshape_star(3, 0, points = 8, r_outer = 1, r_inner = 0.7)
plot(sf::st_sfc(star5, star8))


many <- sfshape_stars(6, points = 5, r_outer = 0.4, r_inner = 0.16)
plot(many)

```
