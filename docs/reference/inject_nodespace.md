# Dynamic Scale Injection for Edge Clipping

Utility function for **RGraphSpace** that enables edge layers to scan
adjacent nodes and determine their dimensions. This information is used
to compute arrow clipping offsets, preventing edge geometry from
overlapping node symbols.

## Usage

``` r
inject_nodespace(...)
```

## Arguments

- ...:

  Additional parameters passed to other methods (currently ignored).

## Value

An object of class `inject_nodespace`, which interacts with the ggplot2
`+` operator.

## Details

This function operates in two stages within the `ggplot2` workflow:

1.  **Capture:** It scans the `plot` layers for a
    [GeomNodeSpace](https://sysbiolab.github.io/RGraphSpace/reference/GeomNodeSpace.md)
    to extract both mapping variables (from
    [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html)) and
    static parameters (specifically `size` and `stroke`).

2.  **Injection:** It locates
    [GeomEdgeSpace](https://sysbiolab.github.io/RGraphSpace/reference/GeomEdgeSpace.md)
    layers and injects scale rules, captured mappings, and fixed
    parameters into the geometry parameters.

This "lazy injection" calculates edge clipping based on the actual
scales used by the nodes, even if scales are defined after the layers.

**Note:** `inject_nodespace()` must be called last in the `ggplot` chain
to allow the function to correctly scan all previously added layers and
scales.

## See also

[geom_edgespace](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md),
[geom_nodespace](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md)

## Examples

``` r
library(RGraphSpace)
library(igraph)
library(ggplot2)

# Generate a toy star graph
gtoy1 <- make_star(15, mode="out")

# Set node and edge attributes
V(gtoy1)$my_node_var <- runif(vcount(gtoy1), 1, 20)
E(gtoy1)$my_edge_var <-  runif(ecount(gtoy1), 1, 20)

# Create a GraphSpace object with a circular layout
gs <- GraphSpace(gtoy1, layout = layout_in_circle(gtoy1))
#> Validating the 'igraph' object...
#> Vertex attribute 'name' missing; assigning names... 
#> Creating a 'GraphSpace' object...

if (FALSE) { # \dontrun{
# Build the plot
# Note that inject_nodespace() is called at the end to
# synchronize node sizes with edge clipping.
ggplot() +
  geom_edgespace(aes(colour = my_edge_var), data = gs) +
  geom_nodespace(aes(size = my_node_var), data = gs) +
  scale_size(range = c(2, 15)) +
  inject_nodespace()
} # }
```
