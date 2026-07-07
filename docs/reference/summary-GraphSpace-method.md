# Summarise a GraphSpace object

Prints a structured summary of a `GraphSpace` object, including graph
topology, optional feature data, and spatial boundaries for nodes and,
when present, the background image.

Node boundaries are always drawn from `@graph` (original pixel
coordinates, never modified). Image boundaries reflect `@canvas` after
normalization with `image.space = TRUE`, and `@image` otherwise. When
normalized, both boundary lines show the source range and `[0,1]` target
to make the transformation explicit.

## Usage

``` r
# S4 method for class 'GraphSpace'
summary(object, ...)
```

## Arguments

- object:

  A `GraphSpace` object.

- ...:

  Currently unused; present for S4 generic compatibility.

## Value

Invisibly returns `object`, allowing the call to be used inside a
pipeline without side effects beyond the printed output.

## See also

[`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md),
[`normalizeGraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/normalizeGraphSpace-methods.md)
