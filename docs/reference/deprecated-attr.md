# Deprecated attribute-deletion helpers

Superseded by assigning `NULL` through
[`gs_vertex_attr`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-attributes.md)
and
[`gs_edge_attr`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-attributes.md)
functions, for example:

    gs_vertex_attr(gs, "a_node_var") <- NULL
    gs_edge_attr(gs, "an_edge_var") <- NULL

## Usage

``` r
gs_delete_v_attr(...)

gs_delete_e_attr(...)
```

## Details

These functions are now defunct and always error.
