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

## Examples

``` r
library(RGraphSpace)

# Load a demo igraph and create a GraphSpace object
data('gtoy1', package = 'RGraphSpace')
gs <- GraphSpace(gtoy1)
#> Validating the 'igraph' object...
#> Ignoring graph-level attributes: 'name', 'mode', 'center'
#> Creating a 'GraphSpace' object...

# A feature matrix with node identifiers as row names
feats <- matrix(as.numeric(seq_len(gs_vcount(gs) * 3)), ncol = 3,
  dimnames = list(names(gs), c("geneA", "geneB", "geneC")))

# Add features (rows are matched and reordered to the nodes)
gs <- gs_add_features(gs, feats)
gs_features(gs)
#> [1] "geneA" "geneB" "geneC"

# Fetch all features, or a subset as a data.frame
gs_fetch_features(gs)
#> 5 x 3 Matrix of class "dgeMatrix"
#>    geneA geneB geneC
#> n1     1     6    11
#> n2     2     7    12
#> n3     3     8    13
#> n4     4     9    14
#> n5     5    10    15
gs_fetch_features(gs, vars = c("geneA", "geneC"), as_df = TRUE)
#>    geneA geneC
#> n1     1    11
#> n2     2    12
#> n3     3    13
#> n4     4    14
#> n5     5    15
```
