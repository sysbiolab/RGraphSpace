# Create a GraphSpace object

`GraphSpace` is the main constructor for
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-class.md)
objects.

## Usage

``` r
# S4 method for class 'ANY'
GraphSpace(g, layout = NULL, simplify = TRUE, verbose = TRUE, ...)

# S4 method for class 'data.frame'
GraphSpace(g, verbose = TRUE, ...)
```

## Arguments

- g:

  A graph object inheriting from the
  [igraph](https://r.igraph.org/reference/aaa-igraph-package.html) class
  (such as `igraph` and `tbl_graph`) or a `data.frame` used to
  initialize a `GraphSpace` object. If a graph is provided, it should
  include vertex coordinates in `x` and `y` attributes, and vertex
  labels in the `name` attribute. If a `data.frame` is provided, it must
  contain at least `x` and `y` columns representing the node
  coordinates; additional columns will be treated as vertex attributes.
  For graphs requiring edge definitions, use the `igraph`
  initialization.

- layout:

  An optional numeric matrix with two columns for `x` and `y` vertex
  coordinates. If provided, it overrides coordinates in `g`.

- simplify:

  A logical value. If `TRUE` (default), removes loops and multiple edges
  (see [simplify](https://r.igraph.org/reference/simplify.html)).

- verbose:

  A logical value. If `TRUE` (default), displays detailed messages.

- ...:

  Additional arguments passed to the `GraphSpace` constructor.

## Value

A
[GraphSpace](https://sysbiolab.github.io/RGraphSpace/reference/GraphSpace-class.md)
class object.

## Details

`GraphSpace` objects are designed to bridge the gap between network
analysis (via `igraph`) and high-quality visualization (via `ggplot2`).
The constructor ensures that all necessary aesthetics for
[`geom_graphspace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_graphspace.md)
are pre-processed and validated.

**Coordinate System and Normalization:** By default, the constructor
expects coordinates in the `x` and `y` vertex attributes, along with
unique IDs in the `name` vertex attribute. If these are not provided,
the constructor will generate sequential IDs and assign a layout using
the [`layout_nicely`](https://r.igraph.org/reference/layout_nicely.html)
function. These coordinates define the relative positioning of nodes.
For optimal rendering, it is recommended to pass the object through
[`normalizeGraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/normalizeGraphSpace-methods.md)
after construction. This converts vertex positions to Normalized Parent
Coordinates (NPC), ensuring the graph remains centered and scaled
relative to the plotting area.

**Data Structure:** The resulting object stores nodes and edges in
separate internal slots, preserving metadata such as `nodeSize` and
`edgeColor`. If an `igraph` object is provided without specific styling
attributes, `GraphSpace` will assign the default values defined in the
[`geom_graphspace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_graphspace.md)
aesthetics. Users can also specify custom variables in the input graph
to be used as aesthetics within the `ggplot2` grammar.

**Arrowhead Mapping:** The `arrowType` attribute (see *Arrowhead types*
section) sets the glyphs drawn at each edge end, as integer codes (e.g.
`1`), basic token codes (e.g. `"-->"`), or extended token codes (e.g.
`"04|->03"`). This is useful for assigning interaction types in directed
or undirected graphs (e.g., activation vs. inhibition).

## Vertex attributes

The following attributes in `g` are evaluated by the constructor:

|  |  |
|----|----|
| `nodeSize` | Numeric `[0, 100]`, representing % of the plotting space. |
| `nodeShape` | Integer code `[0-25]`; see [points](https://rdrr.io/r/graphics/points.html). |
| `nodeColor` | A valid color name or hexadecimal code. |
| `nodeLineWidth` | Border thickness; see [gpar](https://rdrr.io/r/grid/gpar.html). |
| `nodeLineColor` | A valid color name or hexadecimal code. |
| `nodeLabel` | Character string (`NA` will omit labels). |
| `nodeLabelSize` | Font size in `pts`; see [gpar](https://rdrr.io/r/grid/gpar.html). |
| `nodeLabelColor` | A valid color name or hexadecimal code. |

## Edge attributes

The following attributes in `g` are evaluated by the constructor:

|  |  |
|----|----|
| `edgeLineWidth` | Edge thickness; see [`gpar`](https://rdrr.io/r/grid/gpar.html). |
| `edgeColor` | A valid color name or hexadecimal code. |
| `edgeLineType` | Line style (e.g., "solid", "dashed"); see [`gpar`](https://rdrr.io/r/grid/gpar.html). |
| `arrowType` | Arrowhead style (see *Arrowhead types* section). |

## Arrowhead types

Arrowheads and other edge glyphs are set by the `arrowType` attribute,
at three levels, from the simplest to the most expressive: integer
codes, basic token codes, and extended token codes. The levels can be
mixed in the same attribute (e.g. `c("1", "-1", "01<->01")`).

In directed graphs, arrows follow the edge list orientation by default,
representing forward directions (*e.g.*, `A -> B`). While undirected
graphs do not show arrows by default, specific styles can be manually
assigned for detailed visualization, including forward, backward, or
bidirectional arrowheads.

### Integer codes and basic token codes:

Integer codes select the basic forms, built from arrows and bars; each
has an equivalent basic token code, which draws the same form as a
picture of the edge. In directed graphs only the end glyph is drawn, so
only three codes apply.

Directed graphs (A -\> B):

|             |           |                 |
|-------------|-----------|-----------------|
| **Integer** | **Token** | **Description** |
| `0`         | `"---"`   | No arrow        |
| `1`         | `"-->"`   | Forward arrow   |
| `-1`        | `"--|"`   | Forward bar     |

Undirected graphs (A – B):

|             |           |                              |
|-------------|-----------|------------------------------|
| **Integer** | **Token** | **Description**              |
| `0`         | `"---"`   | No arrow                     |
| `1`         | `"-->"`   | Forward arrow                |
| `2`         | `"<--"`   | Backward arrow               |
| `3`         | `"<->"`   | Bidirectional arrow          |
| `4`         | `"|->"`   | Forward arrow / backward bar |
| `-1`        | `"--|"`   | Forward bar                  |
| `-2`        | `"|--"`   | Backward bar                 |
| `-3`        | `"|-|"`   | Bidirectional bar            |
| `-4`        | `"<-|"`   | Backward arrow / forward bar |

### Token codes:

A token code has the form *start*`-`*end*: the `"-"` shaft separates a
start token (drawn at the source end) from an end token (drawn at the
target end). A basic token is `">"` (arrow), `"|"` (bar), or `"-"` (no
glyph). A start token is written mirrored, so the code reads like the
edge itself; `"<"` and `">"` are interchangeable, and codes are stored
in this canonical form. In directed graphs a start glyph is dropped with
a warning; invalid codes are replaced by the default with a warning.

### Extended token codes:

Adding a modifier, a two-digit glyph number, to a basic token selects an
extended glyph from the glyph collection, e.g. `">01"` (triangle) or
`"|04"` (circle). At the start of a code the modifier comes first, so
`"01<->01"` has triangles at both ends and `"04|->03"` a circle and a
harpoon. Use
[`glyph_list`](https://sysbiolab.github.io/RGraphSpace/reference/glyph_list.md)
to see the available glyphs.

## See also

[`geom_edgespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_edgespace.md),
[`geom_nodespace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_nodespace.md),
[`geom_graphspace`](https://sysbiolab.github.io/RGraphSpace/reference/geom_graphspace.md),
[`plotGraphSpace`](https://sysbiolab.github.io/RGraphSpace/reference/plotGraphSpace-methods.md)

## Author

Sysbiolab.

## Examples

``` r
library(RGraphSpace)
library(igraph)

# Create a star graph
gtoy1 <- make_full_graph(15)

# Custom attributes
V(gtoy1)$nodeSize <- 5
E(gtoy1)$edgeColor <- "red"
E(gtoy1)$arrowType <- "-->"

# Create a GraphSpace
gs <- GraphSpace(gtoy1)
#> Validating the 'igraph' object...
#> Vertex attributes 'x' and 'y' missing; computing layout...
#> Vertex attribute 'name' missing; assigning names... 
#> Ignoring graph-level attributes: 'name', 'loops'
#> Creating a 'GraphSpace' object...
```
