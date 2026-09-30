################################################################################
### Validate igraph for RGraphSpace
################################################################################
.validate_igraph <- function(g, layout = NULL, simplify = TRUE, 
  verbose = FALSE) {
  
  if (!inherits(g, "igraph")) {
    rlang::abort("'g' should be an 'igraph' object.")
  }
  
  if (!is.null(layout)) {
    if (nrow(layout) != vcount(g)) {
      msg <- paste("'layout' must have xy-coordinates",
        "for the exact number of nodes in 'g'")
      rlang::abort(msg)
    } else {
      if(all(c("x","y") %in% colnames(layout))){
        igraph::V(g)$x <- layout[, "x"]
        igraph::V(g)$y <- layout[, "y"]
      } else {
        igraph::V(g)$x <- layout[, 1]
        igraph::V(g)$y <- layout[, 2]
      }
    }
  } else if (!all(c("x", "y") %in% igraph::vertex_attr_names(g))) {
    msg <- paste0("Vertex attributes 'x' and 'y' missing; ",
      "computing layout...")
    if (verbose) rlang::inform(msg)
    layout <- igraph::layout_nicely(g)
    igraph::V(g)$x <- layout[, 1]
    igraph::V(g)$y <- layout[, 2]
  }
  
  if (!("name" %in% igraph::vertex_attr_names(g))) {
    msg <- "Vertex attribute 'name' missing; assigning names... "
    if (verbose) rlang::inform(msg)
    igraph::V(g)$name <- paste0("n", seq_len(igraph::vcount(g)))
  } else {
    vnames <- igraph::V(g)$name
    if(is.vector(vnames) && !is.list(vnames)){
      if(any(is.na(vnames))){
        msg <- "NA values found in vertex attribute 'name'."
        rlang::abort(msg)
      }
      if(!.all_characterValues(vnames)){
        rlang::warn("vertex attribute 'name' converted to character.")
        vnames <- as.character(vnames)
        igraph::V(g)$name <- vnames
      }
    } else {
      msg <- "vertex attribute 'name' should be a character vector."
      rlang::abort(msg) 
    }
    if (anyDuplicated(vnames) > 0){
      rlang::abort("vertex names must be unique.")
    }
  }
  
  if (simplify && !igraph::is_simple(g)) {
    if (verbose) {
      rlang::inform("Simplifying graph...")
      if (igraph::any_loop(g))
        rlang::inform("Removing loops...")
      if (igraph::any_multiple(g)){
        rlang::inform("Merging duplicate edges...")
        rlang::inform("Retaining attributes from the first occurrence.")
      }
    }
    opts <- igraph::igraph_opt("edge.attr.comb")
    opts[[length(opts)]] <- "first"
    g <- igraph::simplify(g, remove.loops = TRUE, remove.multiple = TRUE,
      edge.attr.comb = opts)
  }
  
  if( !("nodeLabel" %in% igraph::vertex_attr_names(g)) ){
    igraph::V(g)$nodeLabel <- igraph::V(g)$name
  }
  
  if( !("nodeSize" %in% igraph::vertex_attr_names(g)) ){
    igraph::V(g)$nodeSize <- .get_default_vatt()[["nodeSize"]]
  }
  
  if( !("arrowType" %in% igraph::edge_attr_names(g)) ){
    if (is_directed(g)) {
      igraph::E(g)$arrowType <- "->"
    } else {
      igraph::E(g)$arrowType <- "--"
    }
  }
  
  # Deprecation: edgeLineColor -> edgeColor
  if( "edgeLineColor" %in% igraph::edge_attr_names(g) ){
    rlang::warn(paste0(
      "Edge attribute 'edgeLineColor' is deprecated as of ",
      "RGraphSpace 1.4.3; use 'edgeColor' instead."),
      .frequency = "once",
      .frequency_id = "edgeLineColor_deprecated"
    )
    if( !("edgeColor" %in% igraph::edge_attr_names(g)) ){
      igraph::E(g)$edgeColor <- igraph::E(g)$edgeLineColor
    }
    g <- igraph::delete_edge_attr(g, "edgeLineColor")
  }
  
  if(verbose){
    d_names <- igraph::graph_attr_names(g)
    if (length(d_names) > 0){
      rlang::inform(sprintf(
        "Ignoring graph-level attribute%s: %s",
        if (length(d_names) == 1) "" else "s",
        .gs_preview(shQuote(d_names), n = 3)
      ))
    } 
  }
  
  g <- .validate_attributes(g)
  
  g
  
}

################################################################################
### Validate graph attributes
################################################################################
.validate_attributes <- function(g){
  g <- .validate_nodes(g)
  g <- .validate_edges(g)
  g <- .validate_graph(g)
  g
}

#-------------------------------------------------------------------------------
.validate_nodes <- function(g) {
  
  # get default attributes
  atts <- c(.get_required_vatt(), .get_default_vatt())
  a_names <- names(atts)
  # check default attributes
  b_names <- a_names[a_names %in% igraph::vertex_attr_names(g)]
  if(length(b_names)>0){
    if (vcount(g) > 0) {
      .validate_vatt(igraph::vertex_attr(g)[b_names])
    }
  }
  
  # put default attributes 1st
  d_names <- igraph::vertex_attr_names(g)
  a_names <- a_names[a_names %in% d_names]
  a_names <- c(a_names, d_names[ ! d_names %in% a_names ])
  igraph::vertex_attr(g) <- igraph::vertex_attr(g)[a_names]
  
  # attributes that require transformation
  g <- .validate_nodeshape(g)
  
  g
}

#-------------------------------------------------------------------------------
.validate_edges <- function(g) {
  
  g <- .remove_hidden_eatt(g)
  
  # get default attributes
  atts <- .get_default_eatt(igraph::is_directed(g))
  a_names <- names(atts)
  # check default attributes
  b_names <- a_names[a_names %in% igraph::edge_attr_names(g)]
  if(length(b_names)>0){
    if (igraph::ecount(g) > 0) {
      .validate_eatt(igraph::edge_attr(g)[b_names])
    }
  }
  
  # put default attributes 1st
  d_names <- igraph::edge_attr_names(g)
  a_names <- a_names[a_names %in% d_names]
  a_names <- c(a_names, d_names[ ! d_names %in% a_names ])
  igraph::edge_attr(g) <- igraph::edge_attr(g)[a_names]
  
  # attributes that require transformation
  g <- .validate_arrowtype(g)
  g <- .validate_linetype(g)
  g
}

#-------------------------------------------------------------------------------
.validate_graph <- function(g) {
  d_names <- igraph::graph_attr_names(g)
  if (length(d_names) > 0) {
    for (at in d_names) {
      g <- igraph::delete_graph_attr(g, name = at)
    }
  }
  g
}

################################################################################
### Default RGraphSpace attributes
################################################################################
.gs_protected_node_cols <- function(ext = FALSE) {
  cols <- c("vertex", "name")
  if(ext) cols <- c(cols, "x", "y", "nodeLabel", "nodeSize")
  cols
}
.gs_protected_edge_cols <- function(ext = FALSE) {
  cols <- c("vertex1", "vertex2", "name1", "name2",
    "curve_weight", "is_multiple", "is_loop")
  if(ext) cols <- c(cols, "arrowType")
  cols
}
#-------------------------------------------------------------------------------
.get_required_vatt <- function() {
  atts <- list("x" = NA, "y" = NA, "name" = NA)
  atts
}
.get_default_vatt <- function() {
  atts <- list(
    "nodeLabel" = NA, "nodeLabelSize" = 3, "nodeLabelColor" = "grey40",
    "nodeShape" = 21, "nodeSize" = 5, "nodeColor" = "grey80", 
    "nodeFillColor" = "grey80", "nodeLineWidth" = 0.5, 
    "nodeLineColor" = "grey20")
  atts
}
.get_default_eatt <- function(is.directed = FALSE) {
  atts <- list("edgeLineType" = "solid", "edgeColor" = "grey80",
    "edgeLineWidth" = 0.5)
  if (is.directed) {
    atts$arrowType <- "->"
  } else {
    atts$arrowType <- "--"
  }
  atts$weight <- 1
  atts
}
# remove internally used intermediate attributes
.remove_hidden_eatt <- function(g){
  atts <- names(.get_default_eatt(igraph::is_directed(g)))
  hidden <- setdiff(names(.get_empty_edgedf()), atts)
  hidden <- hidden[hidden %in% igraph::edge_attr_names(g)]
  if (length(hidden) > 0) {
    for (at in hidden) {
      g <- igraph::delete_edge_attr(g, name = at)
    }
  }
  g
}

################################################################################
### Validate attribute values
################################################################################
.validate_vatt <- function(atts) {
  if (!is.null(atts$x)) {
    .validate_gs_args("numeric_vec", "x", atts$x)
  }
  if (!is.null(atts$y)) {
    .validate_gs_args("numeric_vec", "y", atts$y)
  }
  if (!is.null(atts$name)) {
    .validate_gs_args("allCharacter", "name", atts$name)
  }
  if (!is.null(atts$nodeLabel)) {
    .validate_gs_args("allCharacterOrNa", "nodeLabel", atts$nodeLabel)
  }
  if (!is.null(atts$nodeLabelSize)) {
    .validate_gs_args("numeric_vec", "nodeLabelSize", atts$nodeLabelSize)
    if (min(atts$nodeLabelSize, na.rm = TRUE) <= 0) {
      rlang::abort(
        "'nodeLabelSize' should be a vector of numeric values >0")
    }
  }
  if (!is.null(atts$nodeLabelColor)) {
    .validate_gs_colors("allColors", "nodeLabelColor", atts$nodeLabelColor)
  }
  if (!is.null(atts$nodeSize)) {
    .validate_gs_args("numeric_vec", "nodeSize", atts$nodeSize)
    if (max(atts$nodeSize, na.rm = TRUE) > 100 || 
        min(atts$nodeSize, na.rm = TRUE) < 0) {
      rlang::abort(
        "'nodeSize' should be a vector of numeric values in [0, 100]")
    }
  }
  if (!is.null(atts$nodeShape)) {
    .validate_gs_args("allCharacterOrInteger", "nodeShape", atts$nodeShape)
  }
  if (!is.null(atts$nodeColor)) {
    .validate_gs_colors("allColors", "nodeColor", atts$nodeColor)
  }
  if (!is.null(atts$nodeFillColor)) {
    .validate_gs_colors("allColors", "nodeFillColor", atts$nodeFillColor)
  }
  if (!is.null(atts$nodeLineWidth)) {
    .validate_gs_args("numeric_vec", "nodeLineWidth", atts$nodeLineWidth)
    if (min(atts$nodeLineWidth, na.rm = TRUE) < 0) {
      rlang::abort(
        "'nodeLineWidth' should be a vector of numeric values >=0")
    }
  }
  if (!is.null(atts$nodeLineColor)) {
    .validate_gs_colors("allColors", "nodeLineColor", atts$nodeLineColor)
  }
}
#-------------------------------------------------------------------------------
.validate_eatt <- function(atts) {
  if (!is.null(atts$edgeLineType)) {
    .validate_gs_args("allCharacterOrInteger", "edgeLineType",
      atts$edgeLineType)
  }
  if (!is.null(atts$edgeLineWidth)) {
    .validate_gs_args("numeric_vec", "edgeLineWidth", atts$edgeLineWidth)
    if (min(atts$edgeLineWidth, na.rm = TRUE) <= 0) {
      rlang::abort(
        "'edgeLineWidth' should be a vector of numeric values >0")
    }
  }
  if (!is.null(atts$edgeColor)) {
    .validate_gs_colors("allColors", "edgeColor", atts$edgeColor)
  }
  if (!is.null(atts$arrowType)) {
    .validate_gs_args("allCharacterOrInteger", "arrowType", atts$arrowType)
  }
  if (!is.null(atts$weight)) {
    .validate_gs_args("numeric_vec", "weight", atts$weight)
  }
}

################################################################################
### Transform attribute types
################################################################################

#-------------------------------------------------------------------------------
.validate_nodeshape <- function(g) {
  if (vcount(g) > 0 && "nodeShape" %in% names(vertex_attr(g))) {
    V(g)$nodeShape  <- .transform_nodeshape(V(g)$nodeShape)
  }
  g
}
.transform_nodeshape <- function(vshapes) {
  if (.all_integerValues(vshapes)) {
    vshapes[vshapes > 25] <- 21
    vshapes[vshapes < 0] <- 1
  } else {
    vshapes <- tolower(vshapes)
    pch <- rep(21, length(vshapes))
    pch[grep("circle", vshapes)] <- 21
    pch[grep("ellipse", vshapes)] <- 21
    pch[grep("square", vshapes)] <- 22
    pch[grep("diamond", vshapes)] <- 23
    pch[grep("triangle", vshapes)] <- 24
    pch[grep("rectangle", vshapes)] <- 22
    vshapes <- pch
  }
  vshapes
}

#-------------------------------------------------------------------------------
.validate_linetype <- function(g) {
  if (ecount(g) > 0 && "edgeLineType" %in% names(edge_attr(g))) {
    E(g)$edgeLineType  <- .transform_linetype(E(g)$edgeLineType)
  }
  g
}
.transform_linetype <- function(lty) {
  ltypes <- .linetypes()
  if (.all_integerValues(lty)) {
    lty[!lty %in% ltypes] <- 1
    lty <- ltypes[match(lty, ltypes)]
    lty <- names(lty)
  } else {
    lty <- tolower(lty)
    lty[grep("solid", lty)] <- "solid"
    lty[grep("dotted", lty)] <- "dotted"
    lty[grep("dashed", lty)] <- "dashed"
    lty[grep("long", lty)] <- "longdash"
    lty[grep("two", lty)] <- "twodash"
    is_valid_hex <- grepl("^[0-9a-f]{2,8}$", lty) & nchar(lty) %% 2 == 0
    lty[!lty %in% names(ltypes) & !is_valid_hex] <- "solid"
  }
  lty
}
.linetypes <- function() {
  c('blank' = 0, 'solid' = 1, 'dashed' = 2, 'dotted' = 3,
    'dotdash' = 4, 'longdash' = 5, 'twodash' = 6)
}

#-------------------------------------------------------------------------------
.validate_arrowtype <- function(g) {
  if (ecount(g) > 0 && "arrowType" %in% names(edge_attr(g))) {
    E(g)$arrowType  <- .transform_arrowtype(E(g)$arrowType, is_directed(g))
  }
  g
}

#-------------------------------------------------------------------------------
# Validate arrowType and return the canonical form.
# Accepts integer codes or token codes
.transform_arrowtype <- function(eatt, is_dir = FALSE) {
  
  if (.all_integerValues(eatt)) {
    ## integer codes: validate against the accepted set
    aty <- .int_arrowtypes(is_dir)
    unknown_code <- !eatt %in% aty
    if (any(unknown_code)) {
      invalid <- eatt[unknown_code]
      eatt[unknown_code] <- if (is_dir) 1 else 0
      .arrowtypes_warning(is_dir, invalid)
    }
  } else {
    # translate to tokens
    tk <- .arrowtype_to_tokens(eatt)
    # in directed graphs only the end glyph is drawn: drop start glyphs,
    # keeping the end glyph
    if (is_dir) {
      has_start <- tk[, "start"] != "-"
      if (any(has_start)) {
        rlang::warn(c(
          "!" = sprintf(
            "Start glyphs are not drawn in directed graphs: %s",
            .gs_preview(eatt[has_start], n = 3)),
          "i" = "Dropping the start glyph; the end glyph is kept."
        ))
        tk[has_start, "start"] <- "-"
      }
    }
    # check validity against the glyph vocabulary
    unknown_glyph  <- !.valid_tokens(tk)
    if (any(unknown_glyph)) {
      invalid <- eatt[unknown_glyph]
      bad_tk <- setdiff(tk[unknown_glyph, ], names(.glyph_vocab()))
      replace_tk <- c("-", if (is_dir) ">" else "-")
      tk[unknown_glyph,] <- rep(replace_tk, each = sum(unknown_glyph))
      .arrowtypes_warning(is_dir, invalid, .explain_tokens(bad_tk))
    }
    ## write the tokens back as a canonical code (e.g. "<->")
    eatt <- .tokens_to_arrowtype(tk[, "start"], tk[, "end"])
  }
  
  eatt
  
}

#-------------------------------------------------------------------------------
.arrowtypes_warning <- function(is.dir = FALSE, invalid = "",
  explain = character(0)){
  
  graph_type <- if (is.dir) "directed" else "undirected"
  headline <- sprintf(
    "Invalid 'arrowType' for %s graphs: %s",
    graph_type, .gs_preview(invalid, n = 3))
  default <- if (is.dir) {
    "Using the default arrow-end token: '-->'."
  } else {
    "Using the default start-end token: '---'."
  }
  rlang::warn(c(
    "!" = headline,
    "i" = default,
    explain,
    "i" = "See `glyph_list()` for glyphs to compose tokens.",
    "i" = "Integer codes (-4 to 4) are also accepted; see `?GraphSpace`."
  ))
}
