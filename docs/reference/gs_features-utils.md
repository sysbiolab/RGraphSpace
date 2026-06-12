# Manipulate node features in a GraphSpace object

Utilities for extracting and adding node-associated features stored in
the `fdata` container of a `GraphSpace` object.

## Usage

``` r
gs_fetch_features(x, vars = NULL, as_df = FALSE)

gs_add_features(x, data)
```

## Arguments

- x:

  A `GraphSpace` object.

- vars:

  Character vector specifying feature names to extract. If `NULL`, all
  features are returned.

- as_df:

  Logical. If `TRUE`, returns a `data.frame`. Otherwise returns the
  original backend representation.

- data:

  A matrix-like or `data.frame` object containing node features. Rows
  must correspond to node identifiers.

## Value

- `gs_fetch_features()` returns a matrix-like object or `data.frame`
  containing the selected node features.

- `gs_add_features()` returns a modified `GraphSpace` object.
