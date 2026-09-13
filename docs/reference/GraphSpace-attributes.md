# Attribute utilities for GraphSpace objects

Access and modify individual components of a
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-class.md)
object. Selected igraph methods are applied to the internal graph
representation and propagated to downstream components.

## Usage

``` r
# S4 method for class 'GraphSpace'
gs_vertex_attr(x, name, ..., value)

# S4 method for class 'GraphSpace'
gs_vertex_attr(x, name, ...) <- value

# S4 method for class 'GraphSpace'
gs_edge_attr(x, name, ..., value)

# S4 method for class 'GraphSpace'
gs_edge_attr(x, name, ...) <- value
```

## Arguments

- x:

  A
  [GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-class.md)
  class object

- name:

  Name of the attribute.

- ...:

  Additional arguments passed to extraction methods.

- value:

  Replacement value for the selected slot or attribute.

## Examples

``` r
library(RGraphSpace)
library(igraph)

# Load a demo igraph
data('gtoy1', package = 'RGraphSpace')

# Create a new GraphSpace object
gs <- GraphSpace(gtoy1)
#> Validating the 'igraph' object...
#> Ignoring graph-level attributes: 'name', 'mode', 'center'
#> Creating a 'GraphSpace' object...

#--- Usage of GraphSpace attribute accessors:

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
#> [1] 3 3 3 3 3
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
#> $nodeFillColor
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

# Add a new vertex attribute
gs_vertex_attr(gs, "node_var1") <- rnorm(gs_vcount(gs))

# Delete a vertex attribute
gs_vertex_attr(gs, "node_var1") <- NULL

# Access a specific edge attribute
gs_edge_attr(gs, "edgeColor")
#> [1] "red"   "green" "blue"  "black"

# Replace an entire edge attribute
gs_edge_attr(gs, "edgeLineWidth") <- 1

# Add a new edge attribute
gs_edge_attr(gs, "edge_var1") <- rnorm(gs_ecount(gs))

# Delete an edge attribute
gs_edge_attr(gs, "edge_var1") <- NULL
```
