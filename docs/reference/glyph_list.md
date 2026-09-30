# List available edge glyphs

The edge glyphs available for use in `arrowType` codes. This is the
RGraphSpace glyph vocabulary, discovered from the built-in `Glyph*`
objects at package load.

## Usage

``` r
glyph_list()

# S3 method for class 'gs_glyph_list'
plot(
  x,
  ncol = 3,
  legend_title = "Edge glyphs",
  by_group = FALSE,
  glyph_size = 3,
  key_width = 12,
  text_size = 10,
  ...
)
```

## Arguments

- x:

  A `gs_glyph_list` object, as returned by `glyph_list()`.

- ncol:

  Number of columns of keys (ignored when `by_group = TRUE`).

- legend_title:

  Legend title (ignored when `by_group = TRUE`).

- by_group:

  Logical; if `TRUE`, draw one column per glyph group, each titled with
  the group's name.

- glyph_size, key_width, text_size:

  Sizes passed to
  [`glyph_legend`](https://sysbiolab.github.io/RGraphSpace/reference/glyph_legend.md);
  reduced if needed to fit the plotting area.

- ...:

  Further arguments passed to
  [`glyph_legend`](https://sysbiolab.github.io/RGraphSpace/reference/glyph_legend.md),
  such as `colour`.

## Value

A data frame of class `gs_glyph_list`, one row per token.

## See also

[`glyph_legend`](https://sysbiolab.github.io/RGraphSpace/reference/glyph_legend.md)

## Examples

``` r

# List the glyphs as a data frame
glyphs <- glyph_list()

# Plot the glyphs for visual inspection
plot(glyphs)


# Plot the glyphs in one column per group
plot(glyphs, by_group = TRUE)

```
