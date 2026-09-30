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

## Examples

``` r
data('gtoy1', package = 'RGraphSpace')
gs <- GraphSpace(gtoy1)
#> Validating the 'igraph' object...
#> Ignoring graph-level attributes: 'name', 'mode', 'center'
#> Creating a 'GraphSpace' object...
summary(gs)
#> IGRAPH 5fb8aab DN-- 5 4 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeLabelSize
#> | (v/n), nodeLabelColor (v/c), nodeShape (v/n), nodeSize (v/n),
#> | nodeFillColor (v/c), nodeLineWidth (v/n), nodeLineColor (v/c),
#> | nodeAlpha (v/n), edgeLineType (e/c), edgeColor (e/c), edgeLineWidth
#> | (e/n), arrowType (e/n), edgeAlpha (e/n)
#> + node spatial boundaries: raw graph
#> | x: [-8, 2] (cols)
#> | y: [-4, 2] (rows)

# Printing the object calls summary() through show()
gs
#> A GraphSpace-class object for:
#> IGRAPH 5fb8aab DN-- 5 4 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeLabelSize
#> | (v/n), nodeLabelColor (v/c), nodeShape (v/n), nodeSize (v/n),
#> | nodeFillColor (v/c), nodeLineWidth (v/n), nodeLineColor (v/c),
#> | nodeAlpha (v/n), edgeLineType (e/c), edgeColor (e/c), edgeLineWidth
#> | (e/n), arrowType (e/n), edgeAlpha (e/n)
#> + node spatial boundaries: raw graph
#> | x: [-8, 2] (cols)
#> | y: [-4, 2] (rows)
```
