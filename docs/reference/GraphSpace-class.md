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
  [`as.raster`](https://rdrr.io/r/grDevices/as.raster.html)) used as
  background image.

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
