# Plot GraphSpace objects

Plot GraphSpace objects

## Usage

``` r
# S3 method for class 'GraphSpace'
plot(x, ...)
```

## Arguments

- x:

  A
  [GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-class.md)
  class object.

- ...:

  Additional arguments passed to the
  [`plotGraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/plotGraphSpace-methods.md)
  function.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## See also

[`plotGraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/plotGraphSpace-methods.md)

## Examples

``` r
data('gtoy1', package = 'RGraphSpace')
gs <- GraphSpace(gtoy1)
#> Validating the 'igraph' object...
#> Ignoring graph-level attributes: 'name', 'mode', 'center'
#> Creating a 'GraphSpace' object...
plot(gs)

```
