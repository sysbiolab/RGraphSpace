# Accessors and attribute utilities for GraphSpace objects

Access and modify individual components of a
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-class.md)
object. Selected igraph methods are applied to the internal graph
representation and propagated to downstream node and edge components.

## Usage

``` r
# S4 method for class 'GraphSpace'
names(x)

# S4 method for class 'GraphSpace'
gs_names(x)

# S4 method for class 'GraphSpace'
gs_nodes(x, ...)

# S4 method for class 'GraphSpace'
gs_edges(x)

# S4 method for class 'GraphSpace'
gs_image(x)

# S4 method for class 'GraphSpace'
gs_image(x) <- value

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

# S4 method for class 'GraphSpace'
gs_vcount(x)

# S4 method for class 'GraphSpace'
gs_ecount(x)

# S4 method for class 'GraphSpace'
gs_vertex_attr(x, name, ...)

# S4 method for class 'GraphSpace'
gs_vertex_attr(x, name, ...) <- value

# S4 method for class 'GraphSpace'
gs_edge_attr(x, name, ...)

# S4 method for class 'GraphSpace'
gs_edge_attr(x, name, ...) <- value

# S4 method for class 'GraphSpace'
x$name

# S4 method for class 'GraphSpace'
x$name <- value

# S3 method for class 'GraphSpace'
as.igraph(x, ...)
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

[`vertex_attr`](https://r.igraph.org/reference/vertex_attr.html),
[`edge_attr`](https://r.igraph.org/reference/edge_attr.html),
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
#> Creating a 'GraphSpace' object...

#--- Usage of GraphSpace attribute accessors:

# Vertex names
names(gs)
#> [1] "n1" "n2" "n3" "n4" "n5"

# Vertex attribute names
gs_names(gs)
#>  [1] "vertex"         "x"              "y"              "name"          
#>  [5] "nodeLabel"      "nodeLabelSize"  "nodeLabelColor" "nodeShape"     
#>  [9] "nodeSize"       "nodeColor"      "nodeLineWidth"  "nodeLineColor" 
#> [13] "nodeAlpha"     

# Get a data frame with nodes
gs_nodes(gs)
#>    vertex  x  y name nodeLabel nodeLabelSize nodeLabelColor nodeShape nodeSize
#> n1      1  0  0   n1        V1             8          black        21        8
#> n2      2  2  0   n2        V2             8          black        22        5
#> n3      3 -2  2   n3        V3             8          black        23        5
#> n4      4 -4 -4   n4        V4             8          black        24       10
#> n5      5 -8  0   n5        V5             8          black        25        5
#>    nodeColor nodeLineWidth nodeLineColor nodeAlpha
#> n1       red             1        grey20         1
#> n2   #00ad39             1        grey20         1
#> n3    grey80             1        grey20         1
#> n4 lightblue             1        grey20         1
#> n5      cyan             1        grey20         1

# Get a data frame with edges
gs_edges(gs)
#>   x y xend yend offset_start offset_end vertex1 vertex2 name1 name2
#> 1 0 0    2    0            8          5       1       2    n1    n2
#> 2 0 0   -2    2            8          5       1       3    n1    n3
#> 3 0 0   -4   -4            8         10       1       4    n1    n4
#> 4 0 0   -8    0            8          5       1       5    n1    n5
#>   edgeLineType edgeLineColor edgeLineWidth arrowType edgeAlpha
#> 1        solid           red           0.8         1         1
#> 2           11         green           0.8         1         1
#> 3       dashed          blue           0.8         1         1
#> 4         2124         black           0.8         1         1

# Get vertex count
gs_vcount(gs)
#> [1] 5

# Get edge count
gs_ecount(gs)
#> [1] 4

# Access all vertex attributes
gs_vertex_attr(gs)
#> $x
#> [1]  0  2 -2 -4 -8
#> 
#> $y
#> [1]  0  0  2 -4  0
#> 
#> $name
#> [1] "n1" "n2" "n3" "n4" "n5"
#> 
#> $nodeLabel
#> [1] "V1" "V2" "V3" "V4" "V5"
#> 
#> $nodeLabelSize
#> [1] 8 8 8 8 8
#> 
#> $nodeLabelColor
#> [1] "black" "black" "black" "black" "black"
#> 
#> $nodeShape
#> [1] 21 22 23 24 25
#> 
#> $nodeSize
#> [1]  8  5  5 10  5
#> 
#> $nodeColor
#> [1] "red"       "#00ad39"   "grey80"    "lightblue" "cyan"     
#> 
#> $nodeLineWidth
#> [1] 1 1 1 1 1
#> 
#> $nodeLineColor
#> [1] "grey20" "grey20" "grey20" "grey20" "grey20"
#> 
#> $nodeAlpha
#> [1] 1 1 1 1 1
#> 

# Access a specific vertex attribute
gs_vertex_attr(gs, "nodeLabel")
#>   n1   n2   n3   n4   n5 
#> "V1" "V2" "V3" "V4" "V5" 

# Modify a single value within a vertex attribute
gs_vertex_attr(gs, "nodeSize")["n1"] <- 10

# Replace an entire vertex attribute
gs_vertex_attr(gs, "nodeSize") <- 10

# Alternative syntax using `$` accessor
gs_vertex_attr(gs)$nodeSize <- 10

# Access a specific edge attribute
gs_edge_attr(gs, "edgeLineColor")
#> [1] "red"   "green" "blue"  "black"

# Replace an entire edge attribute
gs_edge_attr(gs, "edgeLineWidth") <- 1

# Alternative syntax using `$` for edge attributes
gs_edge_attr(gs)$edgeLineWidth <- 3

# Add an image and rescale graph coordinates to image space
# Images may be provided as a raster or numeric matrix
gs_image(gs) <- as_colorraster(volcano)
gs <- normalizeGraphSpace(gs, image.space = FALSE)
#> Normalizing node coordinates to graph space...
```
