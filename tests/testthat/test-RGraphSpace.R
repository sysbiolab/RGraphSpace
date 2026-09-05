
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
  pad <- match.arg(pad); parity <- match.arg(parity)
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
# edge geom's draw_panel (via .geom_remap_edge_offsets / .geom_adj_edge_offsets)
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
  expect_true(RGraphSpace:::.has_fdata(gs))
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
  expect_true(RGraphSpace:::.has_fdata(gs))
})

