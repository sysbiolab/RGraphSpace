# Update a GraphSpace object

Updates `GraphSpace` objects serialized from previous package versions,
adding any missing slots with default values.

## Usage

``` r
# S4 method for class 'GraphSpace'
updateGraphSpace(x, verbose = TRUE)
```

## Arguments

- x:

  A `GraphSpace` object.

- verbose:

  Logical; if `TRUE`, reports which slots were added.

## Value

An updated `GraphSpace` object.

## Examples

``` r
data('gtoy1', package = 'RGraphSpace')
gs <- GraphSpace(gtoy1)
#> Validating the 'igraph' object...
#> Ignoring graph-level attributes: 'name', 'mode', 'center'
#> Creating a 'GraphSpace' object...

# Objects built with the current version are returned unchanged
gs <- updateGraphSpace(gs)
```
