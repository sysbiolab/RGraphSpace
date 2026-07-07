# Accessors for fetching slots from a GraphSpace object

`getGraphSpace` retrieves information from individual slots available in
a GraphSpace object.

## Usage

``` r
# S4 method for class 'GraphSpace'
getGraphSpace(gs, what = "graph")
```

## Arguments

- gs:

  A preprocessed
  [GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-class.md)
  class object

- what:

  A single character value specifying which slot to retrieve from a
  'GraphSpace' object. Options: "graph", "nodes", "edges", "pars",
  "misc", "image", "canvas", and "fdata".

## Value

Content from slots in the
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-class.md)
object.

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

# Get the 'graph' slot in gs
getGraphSpace(gs, what = 'graph')
#> IGRAPH b2723e1 DN-- 5 4 -- 
#> + attr: x (v/n), y (v/n), name (v/c), nodeLabel (v/c), nodeLabelSize
#> | (v/n), nodeLabelColor (v/c), nodeShape (v/n), nodeSize (v/n),
#> | nodeColor (v/c), nodeLineWidth (v/n), nodeLineColor (v/c), nodeAlpha
#> | (v/n), edgeLineType (e/c), edgeLineColor (e/c), edgeLineWidth (e/n),
#> | arrowType (e/n), edgeAlpha (e/n)
#> + edges from b2723e1 (vertex names):
#> [1] n1->n2 n1->n3 n1->n4 n1->n5
```
