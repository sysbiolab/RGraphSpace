# Update a GraphSpace object

Updates `GraphSpace` objects serialized from previous package versions,
adding any missing slots with default values.

## Usage

``` r
# S4 method for class 'GraphSpace'
updateGraphSpace(x, verbose = FALSE)
```

## Arguments

- x:

  A `GraphSpace` object.

- verbose:

  Logical; if `TRUE`, reports which slots were added.

## Value

An updated `GraphSpace` object.
