# Glyph mode of arrowType codes

For each `arrowType` code, which edge ends carry a glyph, as a numeric
mode.

## Usage

``` r
glyph_mode(arrowType)
```

## Arguments

- arrowType:

  A vector of `arrowType` codes (integer codes or token codes; see
  [`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)).

## Value

An integer vector: `0` no glyph, `1` a glyph at the end only, `2` at the
start only, `3` at both ends. Note that `1` and `2` are the reverse of
igraph's `arrow.mode`, where `1` is a backward arrow.

## Examples

``` r
glyph_mode(c("-->", "<--", "<->", "---", "04|->03"))
#> [1] 1 2 3 0 3
```
