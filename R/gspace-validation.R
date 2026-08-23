
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
            igraph::V(g)$x <- layout[, 1]
            igraph::V(g)$y <- layout[, 2]
        }
    } else if (!all(c("x", "y") %in% igraph::vertex_attr_names(g))) {
        layout <- igraph::layout_nicely(g)
        igraph::V(g)$x <- layout[, 1]
        igraph::V(g)$y <- layout[, 2]
        msg <- paste0("Vertex attributes 'x' and 'y' missing; ",
            "computing layout...")
        if (verbose) rlang::inform(msg)
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
                rlang::abort(msg, call. = FALSE)
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
        g <- igraph::simplify(g, remove.loops = TRUE, remove.multiple = TRUE,
            edge.attr.comb = list(weight = "max", "first"))
    }
    
    if( !("nodeLabel" %in% igraph::vertex_attr_names(g)) ){
        igraph::V(g)$nodeLabel <- igraph::V(g)$name
    }
    
    if( !("nodeSize" %in% igraph::vertex_attr_names(g)) ){
        igraph::V(g)$nodeSize <- .get_default_vatt()[["nodeSize"]]
    }
    
    if( !("arrowType" %in% igraph::edge_attr_names(g)) ){
        if (is_directed(g)) {
            igraph::E(g)$arrowType <- 1
        } else {
            igraph::E(g)$arrowType <- 0
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
    
    return(g)
    
}

################################################################################
### Validate graph attributes
################################################################################
.validate_attributes <- function(g){
    g <- .validate_nodes(g)
    g <- .validate_edges(g)
    g <- .validate_graph(g)
    return(g)
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
    
    return(g)
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
    return(g)
}

#-------------------------------------------------------------------------------
.validate_graph <- function(g) {
    d_names <- igraph::graph_attr_names(g)
    if (length(d_names) > 0) {
        for (at in d_names) {
            g <- igraph::delete_graph_attr(g, name = at)
        }
    }
    return(g)
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
    return(atts)
}
.get_default_vatt <- function() {
    atts <- list(
        "nodeLabel" = NA, "nodeLabelSize" = 3, "nodeLabelColor" = "grey40",
        "nodeShape" = 21, "nodeSize" = 5, "nodeColor" = "grey80", 
        "nodeFillColor" = "grey80", "nodeLineWidth" = 0.5, 
        "nodeLineColor" = "grey20")
    return(atts)
}
.get_default_eatt <- function(is.directed = FALSE) {
    atts <- list("edgeLineType" = "solid", "edgeColor" = "grey80",
        "edgeLineWidth" = 0.5)
    if (is.directed) {
        atts$arrowType <- 1
    } else {
        atts$arrowType <- 0
    }
    atts$weight <- 1
    return(atts)
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
            rlang::abort("'nodeLabelSize' should be a vector of numeric values >0")
        }
    }
    if (!is.null(atts$nodeLabelColor)) {
        .validate_gs_colors("allColors", "nodeLabelColor", atts$nodeLabelColor)
    }
    if (!is.null(atts$nodeSize)) {
        .validate_gs_args("numeric_vec", "nodeSize", atts$nodeSize)
        if (max(atts$nodeSize, na.rm = TRUE) > 100 || min(atts$nodeSize, na.rm = TRUE) < 0) {
            rlang::abort("'nodeSize' should be a vector of numeric values in [0, 100]")
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
            rlang::abort("'nodeLineWidth' should be a vector of numeric values >=0")
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
            rlang::abort("'edgeLineWidth' should be a vector of numeric values >0")
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
    return(g)
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
    return(vshapes)
}

#-------------------------------------------------------------------------------
.validate_arrowtype <- function(g) {
    if (ecount(g) > 0 && "arrowType" %in% names(edge_attr(g))) {
        E(g)$arrowType  <- .transform_arrowtype(E(g)$arrowType, is_directed(g))
    }
    return(g)
}
.transform_arrowtype <- function(eatt, is_dir = FALSE) {
        aty <- .arrowtypes(is_dir)
        if (.all_integerValues(eatt)) {
            idx <- !eatt %in% aty
            if (any(idx)) {
                eatt[idx] <- ifelse(is_dir, 1, 0)
                .arrowtypes_warning(is_dir)
            }
        } else {
            idx <- eatt %in% as.character(aty)
            if (any(idx)) {
                eatt[idx] <- names(aty)[match(eatt[idx], as.character(aty))]
            }
            idx <- !eatt %in% names(aty)
            if (any(idx)) {
                eatt[idx] <- "-->"
                .arrowtypes_warning(is_dir)
            }
            eatt <- aty[eatt]
        }
    return(eatt)
}
.arrowtypes <- function(is.dir = FALSE, unique = FALSE) {
    atp1 <- c(
        "---" = 0, "--" = 0, "-" = 0,
        "-->" = 1, "->" = 1, ">" = 1,
        "<--" = 2, "<-" = 2, "<" = 2,
        "<->" = 3, "<>" = 3,
        "|->" = 4, "|>" = 4)
    atp2 <- c(
        "--|" = -1, "-|" = -1, 
        "|--" = -2, "|-" = -2, 
        "|-|" = -3, "||" = -3,
        "<-|" = -4, "<|" = -4)
    atypes <- c(atp1, atp2)
    if (is.dir) {
        atypes <- atypes[atypes %in% c(-1, 0, 1)]
        if(unique){
            atypes <- atypes[match(unique(atypes), atypes)]
        }
    } else {
        if(unique){
            atp1 <- atp1[match(unique(atp1), atp1)]
            atp2 <- atp2[match(unique(atp2), atp2)]
            atypes <- c(atp1, atp2)
        }
    }
    return(atypes)
}

.arrowtypes_warning <- function(is.dir = FALSE){
    
    atypes <- .arrowtypes(is.dir, unique = TRUE)
    as_bullet <- function(idx) {
        paste(paste0("'", names(atypes)[idx], "' or ", atypes[idx]), 
            collapse = ", ")
    }
    graph_type <- if (is.dir) "directed" else "undirected"
    headline <- sprintf(
        "Invalid 'arrowType' for %s graphs; using default values.", 
        graph_type)
    
    if (is.dir) {
        rlang::warn(c(
            headline,
            "i" = paste("Accepted values:", as_bullet(seq_along(atypes)))
        ))
    } else {
        idx <- atypes >= 0
        rlang::warn(c(
            headline,
            "i" = paste("Accepted values:", as_bullet(idx)),
            "i" = as_bullet(!idx)
        ))
    }
    
}

#-------------------------------------------------------------------------------
.validate_linetype <- function(g) {
    if (ecount(g) > 0 && "edgeLineType" %in% names(edge_attr(g))) {
        E(g)$edgeLineType  <- .transform_linetype(E(g)$edgeLineType)
    }
    return(g)
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
    return(lty)
}
.linetypes <- function() {
    c('blank' = 0, 'solid' = 1, 'dashed' = 2, 'dotted' = 3,
        'dotdash' = 4, 'longdash' = 5, 'twodash' = 6)
}


