#-------------------------------------------------------------------------------
# Test basic constructor
test_that("Check RGraphSpace-class constructor", {
  data("gtoy1", package = "RGraphSpace")
  gs <- GraphSpace(gtoy1)
  expect_true(is(gs, "GraphSpace"))
})

#-------------------------------------------------------------------------------
# Test graph/image alignment (normalizeGraphSpace)
# Each node is built from a burned image pixel; after normalize it must land on
# that pixel in the canvas. Covers x/y pad and even/odd parity.
# LIMITATION: raster arm only -- does NOT exercise the SpatRaster
make_alignment_case <- function(pad = c("row", "col"), 
  parity = c("even", "odd")) {
  pad <- rlang::arg_match(pad); parity <- rlang::arg_match(parity)
  vol <- volcano
  vol[which(volcano == quantile(volcano, 0.85), arr.ind = TRUE)] <- 0
  i <- if (parity == "even") 1L else 2L
  rg <- range(which(vol == 0, arr.ind = TRUE)[, pad])
  win <- seq(rg[1] - 1, rg[2] + i)
  vol <- if (pad == "col") vol[, win] else vol[win, ]
  coords <- which(vol == 0, arr.ind = TRUE)
  image <- as.raster( vol/max(vol) )
  landmark <- "red"
  image[vol==0] <- landmark
  list(image = image, coords = coords, landmark = landmark)
}

test_that("Check normalizeGraphSpace() node/image alignment", {
  for (pad in c("row", "col")) for (parity in c("even", "odd")) {
    
    info <- paste0("pad=", pad, " parity=", parity)
    
    cs <- make_alignment_case(pad, parity)
    
    # single node placed on the sentinel cell (same pixel by construction)
    g <- igraph::make_empty_graph(n = nrow(cs$coords))
    igraph::V(g)$y <- cs$coords[, "row"]
    igraph::V(g)$x <- cs$coords[, "col"]
    # igraph::V(g)$nodeFillColor <- NA
    
    gs <- GraphSpace(g, verbose = FALSE)
    gs_image(gs) <- cs$image
    gs <- suppressMessages(normalizeGraphSpace(gs, verbose = FALSE))
    
    # plotGraphSpace(gs, add.image = TRUE)
    
    # locate the sentinel in the normalized canvas
    r <- as.matrix(gs_image(gs))
    nr <- nrow(r); nc <- ncol(r)
    hits <- which(r == cs$landmark, arr.ind = TRUE)
    
    # convert its canvas position to normalized [0,1] (raster row 1 = top)
    lx <- (hits[, "col"] - 0.5) / nc
    ly <- 1 - (hits[, "row"] - 0.5) / nr
    
    # node must sit on its own landmark, within ~1px
    nodes <- gs_nodes(gs)
    d <- sqrt((lx - nodes$x)^2 + (ly - nodes$y)^2)
    tol <- 1.5 / max(nr, nc)
    expect_lt(max(d), tol, label = paste("node-to-landmark distance,", info))
    
  }
})

#-------------------------------------------------------------------------------
# Test rotate/flip/transpose
# (a) explicit coordinate values on a small asymmetric graph, and
# (b) inverse composition restores the original (nodes AND image);
# flip/transpose are self-inverse; four 90-deg rotations return to start.
xy <- function(gs) cbind(gs@nodes$x, gs@nodes$y)
make_gs_image <- function() {
  g <- igraph::make_empty_graph(n = 3)
  igraph::V(g)$x <- c(1, 5, 2)
  igraph::V(g)$y <- c(1, 2, 8)
  igraph::V(g)$name <- c("a","b","c")
  gs <- GraphSpace(g)
  gs_image(gs) <- as_colorraster(matrix(1:12, nrow = 3))
  gs
}

test_that("flip is self-inverse (nodes and image restored)", {
  gs <- make_gs_image()
  once  <- flipGraphSpace(gs, verbose = FALSE)
  twice <- flipGraphSpace(once, verbose = FALSE)
  expect_identical(xy(twice), xy(gs))
  expect_identical(as.matrix(gs_image(twice)),
    as.matrix(gs_image(gs)))
})

test_that("transpose is self-inverse", {
  gs <- make_gs_image()
  gs_r <- transposeGraphSpace(
    transposeGraphSpace(gs, verbose = FALSE), verbose = FALSE)
  expect_identical(xy(gs_r), xy(gs))
  expect_identical(gs_image(gs_r), gs_image(gs))
})

test_that("four 90-degree rotations return the original", {
  gs <- make_gs_image()
  gs_r <- gs; for (k in 1:4) gs_r <- rotateGraphSpace(gs_r, verbose = FALSE)
  expect_identical(xy(gs_r), xy(gs))
  expect_identical(gs_image(gs_r), gs_image(gs))
})

#-------------------------------------------------------------------------------
# Test edge clipping
# Edge endpoints are clipped to the node boundary, so a segment stops at each
# node's edge rather than its center. The clipped geometry is computed in the
# edge grob at draw time (via .geom_adj_node_offsets1 / .geom_adj_node_offsets2)
# and only materializes in the grob, so this test reads endpoints from the
# rendered edge grob (edges.segments) and compares them to node centers taken
# from gs@nodes. Node size is set per-vertex (V(g)$nodeSize); larger nodes clip
# their end further.
edge_endpoints <- function(p) {
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  ft <- grid::grid.force(ggplot2::ggplotGrob(p))
  grid::grid.newpage(); grid::grid.draw(ft)
  eg <- grid::getGrob(ft, "edges.segments", grep = TRUE, global = TRUE)
  stopifnot(inherits(eg, "segments"))
  c(x0 = grid::convertX(eg$x0, "npc", TRUE),
    x1 = grid::convertX(eg$x1, "npc", TRUE),
    y0 = grid::convertY(eg$y0, "npc", TRUE),
    y1 = grid::convertY(eg$y1, "npc", TRUE))
}

make_gs_clipping <- function(sizes = c(5, 20)) {
  g <- igraph::make_empty_graph(n = 2, directed = FALSE)
  g <- igraph::add_edges(g, c(1, 2))
  igraph::V(g)$x <- c(0.25, 0.75); igraph::V(g)$y <- c(0.5, 0.5)
  igraph::V(g)$name <- c("a", "b")
  igraph::V(g)$nodeSize <- sizes
  igraph::E(g)$arrowType <- 3
  suppressMessages(normalizeGraphSpace(GraphSpace(g, verbose = FALSE), verbose = FALSE))
}

test_that("edge endpoints clip to node boundary and respond to node size", {
  gs <- make_gs_clipping(c(5, 20))
  e  <- edge_endpoints(plotGraphSpace(gs))
  nodes <- gs_nodes(gs)
  nx <- sort(nodes$x) # measured node centers, same frame
  
  # 1. horizontal edge -> y endpoints equal (clip is x-only)
  expect_equal(e[["y0"]], e[["y1"]])
  
  # 2. both ends clipped INWARD: each endpoint sits between the two node centers
  expect_gt(min(e[["x0"]], e[["x1"]]), nx[1])
  expect_lt(max(e[["x0"]], e[["x1"]]), nx[2])
  
  # 3. asymmetry: node b (size 20) clips its end more than node a (size 5)
  gap_a <- min(e[["x0"]], e[["x1"]]) - nx[1]
  gap_b <- nx[2] - max(e[["x0"]], e[["x1"]])
  expect_gt(gap_b, gap_a)
})

test_that("larger nodes clip edges further (size response)", {
  small <- gs_ep <- edge_endpoints(plotGraphSpace(make_gs_clipping(c(5, 5))))
  big <- edge_endpoints(plotGraphSpace(make_gs_clipping(c(20, 20))))
  # both ends pulled further in -> segment shortens from both sides
  expect_gt(min(big[["x0"]], big[["x1"]]), min(small[["x0"]], small[["x1"]]))
  expect_lt(max(big[["x0"]], big[["x1"]]), max(small[["x0"]], small[["x1"]]))
})

#-------------------------------------------------------------------------------
# Test the @uuid layer-compatibility guard (inject_nodespace)
# Same-source layers inject silently; different source but same vertex names
# falls back to vertex-id matching (message).
test_that("same-source layers inject silently (matched UUID)", {
  gs <- make_gs_clipping()
  expect_silent(
    ggplot2::ggplot() +
      geom_edgespace(data = gs) +
      geom_nodespace(ggplot2::aes(size = nodeSize), data = gs) +
      ggplot2::scale_size(range = c(3, 9)) +
      inject_nodespace()
  )
})

test_that("cross-source syncs by vertex id (message)", {
  # different uuid, same vertices
  gs1 <- make_gs_clipping(); gs2 <- make_gs_clipping()
  expect_message(
    ggplot2::ggplot() +
      geom_edgespace(data = gs1) +
      geom_nodespace(ggplot2::aes(size = nodeSize), data = gs2) +
      ggplot2::scale_size(range = c(3, 9)) +
      inject_nodespace(),
    "vertex IDs"
  )
})

#-------------------------------------------------------------------------------
# tests/testthat/test-constructor-edge-cases.R
# Characterization tests: lock in current (correct) constructor behavior for
# structural edge cases. Values captured from live runs, not derived.

test_that("0 vertices", {
  gs <- suppressMessages(GraphSpace(igraph::make_empty_graph(n = 0)))
  expect_s4_class(gs, "GraphSpace")
  nodes <- gs_nodes(gs)
  edges <- gs_edges(gs)
  expect_equal(nrow(nodes), 0L)
  expect_equal(nrow(edges), 0L)
})

test_that("1 vertex, no edges", {
  g <- igraph::make_empty_graph(n = 1)
  gs <- suppressMessages(GraphSpace(g))
  nodes <- gs_nodes(gs)
  edges <- gs_edges(gs)
  expect_equal(nrow(nodes), 1L)
  expect_equal(nrow(edges), 0L)
})

test_that("no vertex names -> auto-assigned", {
  g <- igraph::make_ring(3) # no $name
  gs <- suppressMessages(GraphSpace(g))
  nodes <- gs_nodes(gs)
  expect_true(!is.null(nodes$name))
})

test_that("no layout -> generated", {
  g <- igraph::make_ring(4) # no $x/$y
  gs <- suppressMessages(GraphSpace(g))
  nodes <- gs_nodes(gs)
  expect_true(all(c("x","y") %in% names(nodes)))
  expect_true(all(is.finite(nodes$x))) # a layout was produced
})

test_that("multi-edges kept under simplify = FALSE", {
  g <- igraph::graph_from_edgelist(matrix(c(1,2, 1,2),
    byrow = TRUE, ncol = 2), directed = FALSE)
  gs_keep <- suppressMessages(GraphSpace(g, simplify = FALSE))
  gs_simp <- suppressMessages(GraphSpace(g, simplify = TRUE))
  expect_gt(gs_ecount(gs_keep), gs_ecount(gs_simp))
})

test_that("self-loops kept and flagged under simplify = FALSE", {
  g <- igraph::make_empty_graph(n = 2) |>
    igraph::add_edges(c(1, 1, 1, 2)) # self-loop on v1 + edge v1-v2
  gs <- suppressMessages(GraphSpace(g, simplify = FALSE))
  edges <- gs_edges(gs)
  expect_equal(edges$is_loop, c(TRUE, FALSE)) # loop flagged, normal edge not
  expect_equal(nrow(edges), 2L) # both edges retained
})

#-------------------------------------------------------------------------------
# Minimal SpatialExperiment for testing as.GraphSpace().
# Satisfies exactly what the coercion path touches: an assay (default "counts"),
# 2-col spatialCoords, and shared colnames for id alignment
make_toy_spe <- function(ncells = 3, ngenes = 2) {
  cell_ids <- paste0("c", seq_len(ncells))
  gene_ids <- paste0("g", seq_len(ngenes))
  counts <- matrix(
    seq_len(ngenes * ncells), nrow = ngenes, ncol = ncells,
    dimnames = list(gene_ids, cell_ids)
  )
  coords <- matrix(
    c(seq_len(ncells), seq_len(ncells) * 2), ncol = 2,
    dimnames = list(cell_ids, c("x", "y"))
  )
  assays <- list(counts)
  names(assays) <- "counts"
  spe <- SpatialExperiment::SpatialExperiment(
    assays = assays, spatialCoords = coords)
  spe
}

test_that("SpatialExperiment coercion (integration)", {
  skip_if_not_installed("SpatialExperiment")
  skip_if_not_installed("SummarizedExperiment")
  spe <- make_toy_spe()
  gs <- suppressMessages(as.GraphSpace(spe))
  expect_s4_class(gs, "GraphSpace")
  expect_equal(nrow(gs_nodes(gs)), 3L)
  expect_true(.has_fdata(gs))
})

#-------------------------------------------------------------------------------
# Minimal Seurat (embedding path) for testing as.GraphSpace().
# Needs: an assay with named cells, and a 2-D reduction for Embeddings().
make_toy_seurat <- function(ncells = 3, ngenes = 4) {
  cell_ids <- paste0("c", seq_len(ncells))
  gene_ids <- paste0("g", seq_len(ngenes))
  counts <- matrix(
    seq_len(ngenes * ncells), nrow = ngenes, ncol = ncells,
    dimnames = list(gene_ids, cell_ids)
  )
  obj <- SeuratObject::CreateSeuratObject(counts = counts)
  # a 2-D embedding named so Embeddings(obj) returns an ncells x 2 matrix
  emb <- matrix(
    c(seq_len(ncells), seq_len(ncells) * 2), ncol = 2,
    dimnames = list(cell_ids, c("PC_1", "PC_2"))
  )
  obj[["pca"]] <- SeuratObject::CreateDimReducObject(
    embeddings = emb, key = "PC_", assay = SeuratObject::DefaultAssay(obj)
  )
  obj
}

test_that("Seurat coercion, embedding space (integration)", {
  skip_if_not_installed("SeuratObject")
  seu <- suppressWarnings(make_toy_seurat())
  gs  <- suppressMessages(as.GraphSpace(seu, space = "embedding", layer = "counts"))
  expect_s4_class(gs, "GraphSpace")
  expect_equal(nrow(gs_nodes(gs)), 3L)
  expect_true(.has_fdata(gs))
})

#-------------------------------------------------------------------------------
# Regression tests for adding and subsetting edges and nodes

# Helper: a non-simplified graph with four parallel new1/new2
.make_parallel_gs <- function(simplify = FALSE) {
  # Make a GraphSpace with 5 nodes
  g <- igraph::make_empty_graph(5)
  gs <- GraphSpace(g, simplify = FALSE, verbose = FALSE)
  # Add parallel edges and loops
  gs <- gs_add_edges(gs, c(1,2, 1,2, 1,2, 3,3, 5,5))
  # Add new nodes
  gs <- gs |> gs_add_nodes(data.frame(name = c("new1", "new2")))
  # Add new edges
  gs <- gs_add_edges(gs, data.frame(
    from = c("new1", "new1", "new1", "new1"),
    to   = c("new2", "new2", "new2", "new2"),
    edge_var = c(0, 10, 20, 30)))
  gs
}

test_that("removing parallel edges", {
  gs  <- .make_parallel_gs()
  gs2 <- gs_subset_edges(gs, name1 == "new1" & edge_var > 10)
  e <- gs_edges(gs2)
  # Only new1 & edge_var > 10 rows survive
  expect_equal(nrow(e), 2L)
  expect_true(all(e$name1 == "new1"))
  expect_true(all(e$edge_var > 10))
})

test_that("reordered node subsets keep @nodes, @edges and @graph aligned", {
  for (dir in c(TRUE, FALSE)) {
    g <- igraph::make_graph(c("a","b", "b","c", "c","d", "c","c"), directed = dir)
    igraph::V(g)$x <- c(10, 20, 30, 40); igraph::V(g)$y <- c(1, 3, 2, 4)
    igraph::E(g)$arrowType <- if (dir) "-->" else c("-->", "|->", "-->", "-->")
    gs <- GraphSpace(g, simplify = FALSE, verbose = FALSE)
    s <- gs_subset_nodes(gs, c("c", "b", "a"))
    gs_edge_attr(s, "edgeColor") <- "red"
    expect_identical(igraph::V(s@graph)$name, c("c", "b", "a"))
    expect_identical(s@nodes$name, c("c", "b", "a"))
    expect_true(all(s@nodes$name[s@edges$vertex1] == s@edges$name1))
    expect_true(all(s@nodes$name[s@edges$vertex2] == s@edges$name2))
    e <- gs_edges(s, render = TRUE); n <- gs_nodes(s)
    expect_equal(e$x, n[e$name1, "x"])
    expect_equal(e$yend, n[e$name2, "y"])
    expect_equal(nrow(gs[c("c", "b", "a"), 1]@edges), 1L)
    if (!dir) expect_identical(s@edges$arrowType[1:2], c("<--", "<-|"))
  }
})

test_that("gs_subset_nodes() for node ordering and feature subsetting", {
  g <- igraph::make_graph(c("a","b", "b","c", "c","d"), directed = FALSE)
  igraph::V(g)$x <- 1:4; igraph::V(g)$y <- c(1, 3, 2, 4)
  gs <- GraphSpace(g, verbose = FALSE)
  expect_identical(gs_subset_nodes(gs, c("c", "b", "a"))@nodes$name, 
    c("c", "b", "a"))
  expect_identical(gs_subset_nodes(gs, c("d", "c", "b", "a"))@nodes$name, 
    c("d", "c", "b", "a"))
  expect_identical(gs_subset_nodes(gs, c("a", "b", "c", "d")), gs)
  gs <- gs_add_features(gs, matrix(1:4, 4, 1, dimnames = list(letters[1:4], "F1")))
  expect_identical(colnames(gs_subset_nodes(gs, F1 > 1)@nodes), colnames(gs@nodes))
})

#-------------------------------------------------------------------------------
# Regression tests for gs_add_features() 

make_gs_feat <- function() {
  g <- igraph::make_ring(4)
  igraph::V(g)$name <- c("a", "b", "c", "d")
  suppressMessages(GraphSpace(g, layout = igraph::layout_in_circle(g),
    verbose = FALSE))
}

test_that("gs_add_features() stores features aligned to node order", {
  feats <- matrix(c(1, 2, 3, 4, 10, 20, 30, 40), ncol = 2,
    dimnames = list(c("d", "b", "a", "c"), c("f1", "f2")))
  gs <- gs_add_features(make_gs_feat(), feats)
  fd <- gs_fetch_features(gs)
  expect_identical(rownames(fd), names(gs))
  expect_equal(as.vector(fd[, "f1"]), c(3, 2, 4, 1))
  expect_identical(gs_features(gs), c("f1", "f2"))
})

test_that("gs_fetch_features() subsets variables and returns data.frames", {
  feats <- matrix(c(1, 2, 3, 4, 10, 20, 30, 40), ncol = 2,
    dimnames = list(c("a", "b", "c", "d"), c("f1", "f2")))
  gs <- gs_add_features(make_gs_feat(), feats)
  df <- gs_fetch_features(gs, vars = "f2", as_df = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(df$f2, c(10, 20, 30, 40))
  expect_null(gs_fetch_features(gs, vars = "unknown"))
})

#-------------------------------------------------------------------------------
# Regression tests for annotation_gspace_image()

test_that("annotation_gspace_image() returns a layer for a GraphSpace image", {
  data("gtoy1", package = "RGraphSpace", envir = environment())
  gs <- suppressMessages(GraphSpace(gtoy1, verbose = FALSE))
  gs_image(gs) <- as_colorraster(volcano)
  expect_true(inherits(annotation_gspace_image(gs), "Layer"))
  expect_true(inherits(annotation_gspace_image(gs, opacity = 0.5,
    flip.v = TRUE), "Layer"))
})

test_that("annotation_gspace_image() warns and returns NULL without an image", {
  gs <- suppressMessages(GraphSpace(igraph::make_ring(3), verbose = FALSE))
  expect_warning(res <- annotation_gspace_image(gs))
  expect_null(res)
})

#-------------------------------------------------------------------------------
# Regression tests for theme_*()

test_that("theme_gspace_th*() return a ggplot2 theme", {
  themes <- list(theme_gspace_th0, theme_gspace_th1,
    theme_gspace_th2, theme_gspace_th3)
  for (fn in themes) {
    expect_true(ggplot2::is_theme(fn()[[1]]))
  }
})

test_that("theme_gspace_coords() rejects unknown theme names", {
  expect_error(theme_gspace_coords("th9"), class = "rlang_error")
})

#-------------------------------------------------------------------------------
# Glyph system. Tests go through the public interface (glyph_proto(),
# glyph_list(), glyph_legend(), GraphSpace() and gs_edge_attr()) where it can
# express the behaviour, and use internals only for the collection guardrails
# and the renderer. Expected values are derived from the collection rather
# than hard-coded, so adding a glyph does not break them.

# All gs_glyph objects in the namespace: the glyph collection
.ns_glyphs <- function() {
  ns <- asNamespace("RGraphSpace")
  objs <- mget(ls(ns, all.names = TRUE), envir = ns)
  Filter(function(x) inherits(x, "gs_glyph"), objs)
}

# Assign arrowType codes to an n-edge graph and return what is stored
.store_codes <- function(codes, directed = FALSE) {
  g <- igraph::make_ring(length(codes) + 1L, directed = directed,
    circular = FALSE)
  igraph::E(g)$arrowType <- codes
  gs_edge_attr(GraphSpace(g), "arrowType")
}

#--- Collection guardrails: these turn a broken or clashing contributed glyph
#--- into a failing test, so the package cannot be released until it is fixed

test_that("the glyph collection is valid, exported, and token-unique", {
  glyphs <- .ns_glyphs()
  expect_gt(length(glyphs), 0L)
  for (nm in names(glyphs)) {
    expect_no_error(.validate_gs_glyph(glyphs[[nm]]))
  }
  tokens <- vapply(glyphs, function(g) g$token, character(1))
  expect_false(anyDuplicated(tokens) > 0L)
  expect_true(all(names(glyphs) %in% getNamespaceExports("RGraphSpace")))
  expect_setequal(names(.glyph_vocab()), tokens)
  expect_true(all(c("-", ">", "|") %in% tokens))  # the basic glyphs
})

test_that("token parity matches fill: odd filled, even open", {
  # a filled glyph takes an odd number and is followed by its open form at
  # the next (even) number; an open glyph takes an even number
  df <- glyph_list()
  df <- df[df$group != "basic", ]
  num <- as.integer(substring(df$token, 2L))
  filled <- df$draw %in% c("polygon", "circle")
  
  bad <- df$token[filled != (num %% 2L == 1L)]
  expect_length(bad, 0L)
  
  twin <- sprintf("%s%02d", substr(df$token[filled], 1L, 1L), 
    num[filled] + 1L)
  orphan <- df$token[filled][!twin %in% df$token[!filled]]
  expect_length(orphan, 0L)
})

test_that("glyph offsets end the edge line within the glyph", {
  # an offset is 0 or a point back along the glyph, within its extent
  for (g in .ns_glyphs()) {
    if (nrow(g$shape) == 0L) next
    expect_true(g$offset <= 0 && g$offset >= min(g$shape[, 1]) - 1e-8,
      info = g$token)
  }
  bar <- rbind(c(0, 1), c(0, -1))
  expect_error(glyph_proto(bar, token = "|90", draw = "segments",
    offset = 0.5))                  # not beyond the reference point
  expect_error(glyph_proto(bar, token = "|90", draw = "segments",
    offset = c(-1, -2)))            # a single number
  
  # the renderer stops the line by the offset, scaled by the glyph size
  open <- Filter(function(g) g$offset < 0, .ns_glyphs())[[1]]
  edges <- data.frame(arrowTokenStart = "-", arrowTokenEnd = open$token,
    arrow_size = 0.05, stringsAsFactors = FALSE)
  expect_equal(.glyph_line_trim(edges, "end", 1),
    -open$offset * 0.05)
  expect_equal(.glyph_line_trim(edges, "start", 1), 0)
})

test_that("the primitive table covers exactly the accepted draw types", {
  prim <- names(.draw_primitives)
  choices <- eval(formals(glyph_proto)$draw)
  expect_setequal(prim, choices)
})

#--- Defining glyphs

test_that("glyph_proto() builds glyphs and derives the group from the token", {
  bar <- rbind(c(0, 1), c(0, -1))
  g <- glyph_proto(bar, token = "|90", draw = "segments")
  expect_s3_class(g, "gs_glyph")
  expect_equal(g$group, "tee-like")
  expect_equal(g$name, "|90")  # the name defaults to the token
  expect_equal(glyph_proto(bar, token = ">90", draw = "segments")$group,
    "vee-like")
  expect_equal(glyph_proto(bar, token = "|", draw = "segments")$group,
    "basic")
  # the empty glyph: no points, token "-"
  expect_s3_class(glyph_proto(matrix(numeric(0), 0, 2), token = "-"),
    "gs_glyph")
})

test_that("glyph_proto() rejects malformed geometry, tokens, and names", {
  bar <- rbind(c(0, 1), c(0, -1))
  # geometry
  expect_error(glyph_proto(rbind(c(0, 1), c(0, 0), c(0, -1)),
    token = "|90", draw = "segments"))     # odd number of points
  expect_error(glyph_proto(rbind(c(0, 0), c(1, 1)),
    token = "|91", draw = "circle"))       # a circle takes one point
  expect_error(glyph_proto(rbind(c(0, 1), c(1, 0)),
    token = "|91", draw = "polygon"))      # too few points
  expect_error(glyph_proto("not a matrix", token = "|90"))
  # token
  expect_error(glyph_proto(bar, draw = "segments"))  # missing
  for (tk in c("07", ">1", ">012", ">T", "a-b", "<07", "|00")) {
    expect_error(glyph_proto(bar, token = tk, draw = "segments"), info = tk)
  }
  expect_error(glyph_proto(bar, token = c("|90", "|92"), draw = "segments"))
  # name
  expect_error(glyph_proto(bar, token = "|90", draw = "segments",
    name = c("a", "b")))
})

test_that("the glyph validator rejects hand-edited objects", {
  # checks glyph_proto() cannot reach, since discovery validates objects
  # that could have been edited after construction
  good <- glyph_proto(rbind(c(0, 1), c(0, -1)), token = "|90",
    draw = "segments")
  expect_error(.validate_gs_glyph(unclass(good)))
  bad <- unclass(good); bad$name <- NULL; class(bad) <- "gs_glyph"
  expect_error(.validate_gs_glyph(bad))
  bad <- good; bad$draw <- "spline"
  expect_error(.validate_gs_glyph(bad))
  bad <- good; bad$group <- "vee-like"  # "|90" is a tee-like
  expect_error(.validate_gs_glyph(bad))
})

#--- Listing and drawing glyphs

test_that("glyph_list() lists one row per glyph, in group order", {
  df <- glyph_list()
  expect_s3_class(df, "gs_glyph_list")
  expect_true(all(c("token", "name", "group", "draw") %in% names(df)))
  expect_equal(nrow(df), length(.ns_glyphs()))
  expect_false(is.unsorted(match(df$group, c("basic", "vee-like", "tee-like"))))
})

test_that("glyph plots and legends build", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(GlyphArrow, GlyphTriangle1))
  glyphs <- glyph_list()
  expect_no_error(plot(glyphs))
  expect_s3_class(plot(glyphs, by_group = TRUE), "gspace_legend")
  # all three levels of codes, mixed
  leg <- glyph_legend(c(A = "3", B = "-->", C = "01<->01"))
  expect_s3_class(leg, "gspace_legend")
  expect_error(glyph_legend(c(A = "-->99")))
})

#--- Setting glyphs on edges

test_that("token codes are stored in canonical form", {
  expect_equal(
    .store_codes(c("01<->01", "<01->01", "->01", ">01", 
      "04|-|04", "|-|", "00<->00")),
    c("01<->01", "01<->01", "-->01", "-->01", "04|-|04", 
      "|-|", "<->"))
})

test_that("integer codes are kept, or read as token codes when mixed", {
  expect_equal(.store_codes(c(1, -1, 3)), c(1, -1, 3))
  expect_equal(.store_codes(c(1, "01<->01", "-4")),
    c("-->", "01<->01", "<-|"))
})

test_that("directed graphs keep only the end glyph", {
  expect_warning(out <- .store_codes("01<->01", directed = TRUE))
  expect_equal(out, "-->01")
})

test_that("invalid codes fall back to the default with a warning", {
  expect_warning(out <- .store_codes(c("-->99", "01", "-->")), "-->99")
  expect_equal(out, c("---", "---", "-->"))
  expect_warning(out <- .store_codes(c(7, 1)))
  expect_equal(out, c(0, 1))
})

#--- Rendering

test_that("segments colour/width expand to one value per segment", {
  # a segments glyph with 4 points -> 2 segments per instance. Two edges
  # with distinct colours must not bleed into each other (the per-segment
  # expansion bug).
  seg_glyphs <- Filter(function(g) g$draw == "segments" &&
      nrow(g$shape) == 4L, .ns_glyphs())
  skip_if(length(seg_glyphs) == 0L, "no 4-point segments glyph")
  
  # This avoids occasional Rplots.pdf after running the test
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  edges <- data.frame(
    x = c(0.2, 0.6), y = c(0.2, 0.6),
    xend = c(0.4, 0.8), yend = c(0.4, 0.8),
    px0 = 1, py0 = 0, px1 = 1, py1 = 0,
    arrow_size = 0.05, arrowTokenStart = "-", 
    arrowTokenEnd = seg_glyphs[[1]]$token,
    colour = c("#0000FF", "#FF00FF"), linewidth = 1,
    stringsAsFactors = FALSE)
  
  grobs <- .get_glyph_grobs(edges, sz2npc = 1)
  col <- grobs[[1]]$gp$col
  expect_length(col, 4L)         # 2 instances x 2 segments
  expect_equal(col[1], col[2])   # instance 1 solid
  expect_equal(col[3], col[4])   # instance 2 solid
  expect_false(isTRUE(all.equal(col[1], col[3])))  # instances differ
})
