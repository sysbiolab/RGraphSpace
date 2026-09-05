# Convert objects to GraphSpace

S3 generic function for coercing objects into a `GraphSpace` object.

## Usage

``` r
as.GraphSpace(x, ...)

# Default S3 method
as.GraphSpace(x, ...)

# S3 method for class 'igraph'
as.GraphSpace(x, ...)

# S3 method for class 'tbl_graph'
as.GraphSpace(x, ...)

# S3 method for class 'data.frame'
as.GraphSpace(x, ...)

# S3 method for class 'DFrame'
as.GraphSpace(x, ...)

# S3 method for class 'matrix'
as.GraphSpace(x, ...)

# S3 method for class 'SpatialExperiment'
as.GraphSpace(x, assay = "counts", ...)

# S3 method for class 'Seurat'
as.GraphSpace(x, layer = NULL, space = c("embedding", "spatial"), ...)
```

## Arguments

- x:

  An object to be converted.

- ...:

  Additional arguments passed to coercion methods.

- assay:

  Name of the assay in the
  [`SpatialExperiment`](https://rdrr.io/pkg/SpatialExperiment/man/SpatialExperiment.html)
  object from which data should be retrieved (see
  [`assay`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)).

- layer:

  Name of the layer in the
  [`Seurat`](https://satijalab.github.io/seurat-object/reference/Seurat-class.html)
  object from which node data should be retrieved (see
  [`LayerData`](https://satijalab.github.io/seurat-object/reference/Layers.html)).

- space:

  Character specifying the coordinate space used for node geometry.
  Either `"embedding"` or `"spatial"`. See details.

## Value

A `GraphSpace` object.

## Details

Unified entry point for converting graph, spatial, and high-dimensional
data into a `GraphSpace` object.

Graph objects are imported either through native methods or via
[as_tbl_graph](https://tidygraph.data-imaginist.com/reference/tbl_graph.html)
when available.

For **Seurat** objects, coordinate extraction depends on the selected
`space`:

- `space = "embedding"` uses the first two dimensions returned by
  [`Embeddings`](https://satijalab.github.io/seurat-object/reference/Embeddings.html).

- `space = "spatial"` uses tissue coordinates returned by
  [`GetTissueCoordinates`](https://satijalab.github.io/seurat-object/reference/GetTissueCoordinates.html).

Assay data are stored in the `data` slot of the resulting `GraphSpace`
object. Node metadata from `x@meta.data` are appended to the node table.

## See also

[`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-class.md)
