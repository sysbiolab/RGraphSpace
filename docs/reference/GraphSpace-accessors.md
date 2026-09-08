# Accessors for GraphSpace objects

Access and modify individual components of a
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-class.md)
object.

## Usage

``` r
# S4 method for class 'GraphSpace'
names(x)

# S4 method for class 'GraphSpace'
gs_names(x)

# S4 method for class 'GraphSpace'
gs_nodes(x, ...)

# S4 method for class 'GraphSpace'
gs_edges(x, ...)

# S4 method for class 'GraphSpace'
gs_image(x)

# S4 method for class 'GraphSpace'
gs_image(x) <- value

# S4 method for class 'GraphSpace'
gs_image_maxpixels(x)

# S4 method for class 'GraphSpace'
gs_image_maxpixels(x) <- value

# S4 method for class 'GraphSpace'
gs_graph(x)

# S4 method for class 'GraphSpace'
gs_fdata(x)

# S4 method for class 'GraphSpace'
gs_fdata(x) <- value

# S4 method for class 'GraphSpace'
gs_nfeatures(x)

# S4 method for class 'GraphSpace'
gs_features(x)

# S3 method for class 'GraphSpace'
as.igraph(x, ...)

# S4 method for class 'GraphSpace'
gs_vcount(x)

# S4 method for class 'GraphSpace'
gs_ecount(x)

# S4 method for class 'GraphSpace'
gs_scale_factor(x)

# S4 method for class 'GraphSpace'
gs_scale_factor(x) <- value

# S4 method for class 'GraphSpace'
gs_geometry(x, name = "geometry")

# S4 method for class 'GraphSpace'
gs_geometry(x, name = "geometry") <- value

# S4 method for class 'GraphSpace'
x$name

# S4 method for class 'GraphSpace'
x$name <- value
```

## Arguments

- x:

  A
  [GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-class.md)
  class object

- ...:

  Additional arguments passed to extraction methods.

- value:

  Replacement value for the selected slot or attribute.

- name:

  Name of the attribute.

## Value

Updated
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-class.md)
object.

## Details

For `gs_nodes()`, the optional `vars` argument specifies node-associated
features retrieved from the `fdata` container. See also
[`gs_fetch_features`](https://sysbiolab.github.io/RGraphSpace/reference/gs_features-utils.md).

## See also

[`gs_fetch_features`](https://sysbiolab.github.io/RGraphSpace/reference/gs_features-utils.md)

## Examples

``` r
library(RGraphSpace)
library(igraph)
#> 
#> Attaching package: ‘igraph’
#> The following objects are masked from ‘package:stats’:
#> 
#>     decompose, spectrum
#> The following object is masked from ‘package:base’:
#> 
#>     union

# Load a demo igraph
data('gtoy1', package = 'RGraphSpace')

# Create a new GraphSpace object
gs <- GraphSpace(gtoy1)
#> Validating the 'igraph' object...
#> Ignoring graph-level attributes: 'name', 'mode', 'center'
#> Creating a 'GraphSpace' object...

#--- Usage of GraphSpace accessors:

# Vertex names
names(gs)
#> [1] "n1" "n2" "n3" "n4" "n5"

# Vertex attribute names
gs_names(gs)
#>  [1] "vertex"         "x"              "y"              "name"          
#>  [5] "nodeLabel"      "nodeLabelSize"  "nodeLabelColor" "nodeShape"     
#>  [9] "nodeSize"       "nodeFillColor"  "nodeLineWidth"  "nodeLineColor" 
#> [13] "nodeAlpha"     

# Get a data frame with nodes
gs_nodes(gs)
#>    vertex  x  y name nodeLabel nodeLabelSize nodeLabelColor nodeShape nodeSize
#> n1      1  0  0   n1        V1             3          black        21        8
#> n2      2  2  0   n2        V2             3          black        22        5
#> n3      3 -2  2   n3        V3             3          black        23        5
#> n4      4 -4 -4   n4        V4             3          black        24       10
#> n5      5 -8  0   n5        V5             3          black        25        5
#>    nodeFillColor nodeLineWidth nodeLineColor nodeAlpha
#> n1           red             1        grey20         1
#> n2       #00ad39             1        grey20         1
#> n3        grey80             1        grey20         1
#> n4     lightblue             1        grey20         1
#> n5          cyan             1        grey20         1

# Get a data frame with edges
gs_edges(gs)
#>   vertex1 vertex2 name1 name2 edgeLineType edgeColor edgeLineWidth arrowType
#> 1       1       2    n1    n2        solid       red           0.8         1
#> 2       1       3    n1    n3           11     green           0.8         1
#> 3       1       4    n1    n4       dashed      blue           0.8         1
#> 4       1       5    n1    n5         2124     black           0.8         1
#>   edgeAlpha curve_weight is_multiple is_loop
#> 1         1            1       FALSE   FALSE
#> 2         1            1       FALSE   FALSE
#> 3         1            1       FALSE   FALSE
#> 4         1            1       FALSE   FALSE

# Get an igraph object
gs_graph(gs)
#> IGRAPH 5fb8aab DN-- 5 4 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeLabelSize
#> | (v/n), nodeLabelColor (v/c), nodeShape (v/n), nodeSize (v/n),
#> | nodeFillColor (v/c), nodeLineWidth (v/n), nodeLineColor (v/c),
#> | nodeAlpha (v/n), edgeLineType (e/c), edgeColor (e/c), edgeLineWidth
#> | (e/n), arrowType (e/n), edgeAlpha (e/n)
#> + edges from 5fb8aab (vertex names):
#> [1] n1->n2 n1->n3 n1->n4 n1->n5

# Get a data frame with nodes
gs_nodes(gs)
#>    vertex  x  y name nodeLabel nodeLabelSize nodeLabelColor nodeShape nodeSize
#> n1      1  0  0   n1        V1             3          black        21        8
#> n2      2  2  0   n2        V2             3          black        22        5
#> n3      3 -2  2   n3        V3             3          black        23        5
#> n4      4 -4 -4   n4        V4             3          black        24       10
#> n5      5 -8  0   n5        V5             3          black        25        5
#>    nodeFillColor nodeLineWidth nodeLineColor nodeAlpha
#> n1           red             1        grey20         1
#> n2       #00ad39             1        grey20         1
#> n3        grey80             1        grey20         1
#> n4     lightblue             1        grey20         1
#> n5          cyan             1        grey20         1

# Vertex count
gs_vcount(gs)
#> [1] 5

# Edge count
gs_ecount(gs)
#> [1] 4

# Add an image and rescale graph coordinates to image space
# Images may be provided as a raster or numeric matrix
gs_image(gs) <- as_colorraster(volcano)
gs <- normalizeGraphSpace(gs, image.space = FALSE)
#> Normalizing node coordinates to graph space...

# Add a sparse Matrix aligned to nodes
library(Matrix)
mtx <- Matrix(0, gs_vcount(gs), 2)
rownames(mtx) <- names(gs)
colnames(mtx) <- c("feature1","feature2")
gs_fdata(gs) <- mtx

# Feature names
gs_features(gs)
#> [1] "feature1" "feature2"

# Feature count
gs_nfeatures(gs)
#> [1] 2

# Apply a scaling factor to node coordinates
gs_scale_factor(gs) <- 0.1
#> Denormalizing graph coordinates...
# undo scaling 
gs_scale_factor(gs) <- 1

# Add an 'sfc' geometry column (requires the optional 'sf' package)
if (requireNamespace("sf", quietly = TRUE)) {
  pts <- replicate(gs_vcount(gs), sf::st_point(runif(2)), simplify = FALSE)
  gs_geometry(gs) <- sf::st_sfc(pts)
}

# Set a pixel budget for image operations
gs_image_maxpixels(gs) <- 4e+06
```
