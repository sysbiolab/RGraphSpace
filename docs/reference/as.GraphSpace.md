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

# S3 method for class 'Seurat'
as.GraphSpace(x, space = c("embedding", "spatial"), layer = NULL, ...)
```

## Arguments

- x:

  An object to be converted.

- ...:

  Additional arguments passed to methods associated with the selected
  `space`.

- space:

  Character specifying the coordinate space used for node geometry.
  Either `"embedding"` or `"spatial"`. See details.

- layer:

  Name of the layer from which node data should be retrieved (see
  [`LayerData`](https://satijalab.github.io/seurat-object/reference/Layers.html)).

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
