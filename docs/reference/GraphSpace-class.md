# GraphSpace: An S4 class for igraph objects

GraphSpace: An S4 class for igraph objects

## Value

An S4 class object.

## Slots

- `nodes`:

  A data frame containing node coordinates, attributes, and metadata.

- `edges`:

  A data frame containing edge relationships and attributes.

- `graph`:

  An [`igraph`](https://r.igraph.org/reference/aaa-igraph-package.html)
  object representing the graph structure.

- `image`:

  A `raster` object (see
  [`as.raster`](https://rdrr.io/r/grDevices/as.raster.html)) holding the
  original background image as supplied by the user. Never modified
  after construction; always serves as the stable source for
  [`normalizeGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/normalizeGraphSpace-methods.md).

- `canvas`:

  A `raster` object holding the processed, render-ready image produced
  by
  [`normalizeGraphSpace()`](https://sysbiolab.github.io/RGraphSpace/reference/normalizeGraphSpace-methods.md).
  Receives all centering, flipping, and margin adjustments. When this
  slot contains only the empty sentinel, downstream accessors fall back
  to `@image` automatically; see
  [gs_image](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-accessors.md).

- `fdata`:

  A [`Matrix`](https://rdrr.io/pkg/Matrix/man/Matrix.html) object
  storing high-dimensional feature data associated with graph nodes.

- `pars`:

  A list with parameters.

- `misc`:

  A list with intermediate objects for downstream methods.

- `uuid`:

  A Universally Unique Identifier (UUID) for the object instance.

## Constructor

see
[`GraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-methods.md)
constructor.
