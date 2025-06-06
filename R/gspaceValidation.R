
################################################################################
### Validate igraph for RGraphSpace
################################################################################
.validate.igraph <- function(g, layout, verbose = TRUE) {
    if (verbose) message("Validating the 'igraph' object...")
    if (!is(g, "igraph")) {
        stop("'g' should be an 'igraph' object.", call. = FALSE)
    }
    if (!is.null(layout)) {
        if (nrow(layout) != vcount(g)) {
            msg <- paste("'layout' must have xy-coordinates",
                "for the exact number of nodes in 'g'")
            stop(msg, call. = FALSE)
        } else {
            igraph::V(g)$x <- layout[, 1]
            igraph::V(g)$y <- layout[, 2]
        }
    } else if (is.null(igraph::V(g)$x) || is.null(igraph::V(g)$y)) {
        layout <- igraph::layout_nicely(g)
        igraph::V(g)$x <- layout[, 1]
        igraph::V(g)$y <- layout[, 2]
        msg <- paste0("'x' and 'y' vertex attributes are not available;",
            " using a layout algorithm automatically.")
        warning(msg, call. = FALSE)
    }
    if (is.null(igraph::V(g)$name)) {
        msg <- paste0("'name' vertex attribute is not available; ",
            "names will be assigned automatically.")
        warning(msg, call. = FALSE)
        igraph::V(g)$name <- paste0("n", seq_len(igraph::vcount(g)))
    } else if (anyDuplicated(igraph::V(g)$name) > 0) {
        msg <- paste0("'name' vertex attribute should not contain ",
            "duplicated names.")
        stop(msg, call. = FALSE)
    }
    if (is.null(igraph::V(g)$nodeLabel)){
        igraph::V(g)$nodeLabel <- igraph::V(g)$name
    }
    if (!igraph::is_simple(g)) {
        if (verbose && igraph::any_loop(g)) message("Removing loops...")
        if (verbose && igraph::any_multiple(g))
            message("Merging duplicated edges...")
        g <- igraph::simplify(g, remove.loops = TRUE, remove.multiple = TRUE,
          edge.attr.comb = list(weight = "max", "first"))
    }
    g <- .validate.nodes(g)
    g <- .validate.edges(g)
    g <- .validate.graph(g)
    return(g)
}

#-------------------------------------------------------------------------------
.validate.nodes <- function(g) {
    # get default attributes
    atts <- c(.get.required.vatt(), .get.default.vatt())
    a_names <- names(atts)
    # check default attributes
    b_names <- a_names[a_names %in% igraph::vertex_attr_names(g)]
    if(length(b_names)>0){
        if (vcount(g) > 0) {
            .validate.vatt(igraph::vertex_attr(g))
        }
    }
    # add missing default attributes
    c_names <- a_names[!a_names %in% b_names]
    if (length(c_names) > 0) {
        for (at in c_names) {
            igraph::vertex_attr(g, name = at) <- atts[[at]]
        }
    }
    # keep default attributes only
    igraph::vertex_attr(g) <- igraph::vertex_attr(g)[a_names]
    # further checks
    g <- .transform.nodeshape(g)
    return(g)
}

#-------------------------------------------------------------------------------
.validate.edges <- function(g) {
    # get default attributes
    atts <- .get.default.eatt(igraph::is_directed(g))
    a_names <- names(atts)
    # check default attributes
    b_names <- a_names[a_names %in% igraph::edge_attr_names(g)]
    if(length(b_names)>0){
        if (igraph::ecount(g) > 0) {
            .validate.eatt(igraph::edge_attr(g))
        }
    }
    # add missing default attributes
    c_names <- a_names[!a_names %in% b_names]
    if (length(c_names) > 0) {
        for (at in c_names) {
            igraph::edge_attr(g, name = at) <- atts[[at]]
        }
    }
    # keep default attributes only
    igraph::edge_attr(g) <- igraph::edge_attr(g)[a_names]
    # further checks
    g <- .transform.arrowtype(g)
    g <- .transform.linetype(g)
    return(g)
}

#-------------------------------------------------------------------------------
.validate.graph <- function(g) {
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
.get.required.vatt <- function() {
    atts <- list("x" = 0, "y" = 0, "name" = NA)
    return(atts)
}
.get.default.vatt <- function() {
    atts <- list("weight"=1, "nodeLabel" = NA, "nodeLabelSize" = 8,
        "nodeLabelColor" = "grey40", "nodeSize" = 5,
        "nodeShape" = 21, "nodeColor" = "grey80",
        "nodeLineWidth" = 1, "nodeLineColor" = "grey20")
    return(atts)
}
.get.default.eatt <- function(is.directed = FALSE) {
    atts <- list("weight"=1, "edgeLineWidth" = 0.5, 
        "edgeLineColor" = "grey80", "edgeLineType" = "solid")
    if (is.directed) {
        atts$arrowType <- 1
    } else {
        atts$arrowType <- 0
    }
    atts$arrowLength=1
    return(atts)
}

################################################################################
### Validate attribute values
################################################################################
.validate.vatt <- function(atts) {
    if (!is.null(atts$x)) {
        .validate.args("numeric_vec", "x", atts$x)
    }
    if (!is.null(atts$y)) {
        .validate.args("numeric_vec", "y", atts$y)
    }
    if (!is.null(atts$name)) {
        .validate.args("allCharacter", "name", atts$name)
    }
    if (!is.null(atts$nodeLabel)) {
        .validate.args("allCharacterOrNa", "nodeLabel", atts$nodeLabel)
    }
    if (!is.null(atts$nodeLabelSize)) {
        .validate.args("numeric_vec", "nodeLabelSize", atts$nodeLabelSize)
        if (min(atts$nodeLabelSize) < 0) {
            stop("'nodeLabelSize' should be a vector of numeric values >=0", 
              call. = FALSE)
        }
    }
    if (!is.null(atts$nodeLabelColor)) {
        .validate.colors("allColors", "nodeLabelColor", atts$nodeLabelColor)
    }
    if (!is.null(atts$nodeSize)) {
        .validate.args("numeric_vec", "nodeSize", atts$nodeSize)
        if (max(atts$nodeSize) > 100 || min(atts$nodeSize) < 0) {
            stop("'nodeSize' should be a vector of numeric values in [0, 100]", 
              call. = FALSE)
        }
    }
    if (!is.null(atts$nodeShape)) {
        .validate.args("allCharacterOrInteger", "nodeShape", atts$nodeShape)
    }
    if (!is.null(atts$nodeColor)) {
        .validate.colors("allColors", "nodeColor", atts$nodeColor)
    }
    if (!is.null(atts$nodeLineWidth)) {
        .validate.args("numeric_vec", "nodeLineWidth", atts$nodeLineWidth)
        if (min(atts$nodeLineWidth) < 0) {
            stop("'nodeLineWidth' should be a vector of numeric values >=0", 
              call. = FALSE)
        }
    }
    if (!is.null(atts$nodeLineColor)) {
        .validate.colors("allColors", "nodeLineColor", atts$nodeLineColor)
    }
    if (!is.null(atts$weight)) {
        .validate.args("numeric_vec", "weight", atts$weight)
        if (min(atts$weight) < 0) {
            stop("'weight' should be a vector of numeric values >=0", 
                call. = FALSE)
        }
    }
}
#-------------------------------------------------------------------------------
.validate.eatt <- function(atts) {
    if (!is.null(atts$edgeLineWidth)) {
        .validate.args("numeric_vec", "edgeLineWidth", atts$edgeLineWidth)
        if (min(atts$edgeLineWidth) < 0) {
            stop("'edgeLineWidth' should be a vector of numeric values >=0", 
              call. = FALSE)
        }
    }
    if (!is.null(atts$edgeLineColor)) {
        .validate.colors("allColors", "edgeLineColor", atts$edgeLineColor)
    }
    if (!is.null(atts$edgeLineType)) {
        .validate.args("allCharacterOrInteger", "edgeLineType",
            atts$edgeLineType)
    }
    if (!is.null(atts$arrowLength)) {
        .validate.args("numeric_vec", "arrowLength", atts$arrowLength)
        if (min(atts$arrowLength) < 0) {
            stop("'arrowLength' should be a vector of numeric values >=0", 
              call. = FALSE)
        }
    }
    if (!is.null(atts$arrowType)) {
        .validate.args("allCharacterOrInteger", "arrowType", atts$arrowType)
    }
    if (!is.null(atts$weight)) {
        .validate.args("numeric_vec", "weight", atts$weight)
        if (min(atts$weight) < 0) {
            stop("'weight' should be a vector of numeric values >=0", 
                call. = FALSE)
        }
    }
}

################################################################################
### Transform attribute types
################################################################################

#-------------------------------------------------------------------------------
.transform.nodeshape <- function(g) {
    if (vcount(g) > 0 && "nodeShape" %in% names(vertex_attr(g))) {
        vshapes <- V(g)$nodeShape
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
        V(g)$nodeShape <- vshapes
    }
    return(g)
}

#-------------------------------------------------------------------------------
.transform.arrowtype <- function(g) {
    if (ecount(g) > 0 && "arrowType" %in% names(edge_attr(g))) {
        eatt <- E(g)$arrowType
        is_dir <- is_directed(g)
        aty <- .get.arrowtypes(is_dir)
        if (.all_integerValues(eatt)) {
            idx <- !eatt %in% aty
            if (any(idx)) {
                eatt[idx] <- ifelse(is_dir, 1, 0)
                .get.arrowtypes(is_dir, warning = TRUE)
            }
        } else {
            idx <- eatt %in% as.character(aty)
            if (any(idx)) {
                eatt[idx] <- names(aty)[match(eatt[idx], as.character(aty))]
            }
            idx <- !eatt %in% names(aty)
            if (any(idx)) {
                eatt[idx] <- "-->"
                .get.arrowtypes(is_dir, warning = TRUE)
            }
            eatt <- aty[eatt]
        }
        E(g)$arrowType <- eatt
    }
    return(g)
}
.get.arrowtypes <- function(is.dir = FALSE, warning = FALSE) {
    atp1 <- c(
        "---" = 0, "--" = 0, "-" = 0,
        "-->" = 1, "->" = 1, ">" = 1,
        "<--" = 2, "<-" = 2, "<" = 2,
        "<->" = 3, "<>" = 3,
        "|->" = 4, "|>" = 4)
    atp2 <- c(
        "--|" = -1, "-|" = -1, "|" = -1,
        "|--" = -2, "|-" = -2, "|" = -2,
        "|-|" = -3, "||" = -3,
        "<-|" = -4, "<|" = -4)
    atypes <- c(atp1, atp2)
    if (is.dir) {
        atypes <- atypes[atypes %in% c(-1, 0, 1)]
    }
    if (warning) {
        gtype <- ifelse(is.dir, "directed", "undirected")
        msg1 <- paste("'arrowType' value in",
            gtype, "graphs must be one of:\n")
        if (is.dir) {
            msg2 <- atypes[match(unique(atypes), atypes)]
            msg2 <- paste(msg2, paste0("'", names(msg2), "'"), sep = " = ")
            msg2 <- paste(msg2, collapse = ", ")
        } else {
            atp1 <- atp1[match(unique(atp1), atp1)]
            atp2 <- atp2[match(unique(atp2), atp2)]
            atp1 <- paste(atp1, paste0("'", names(atp1), "'"), sep = " = ")
            atp2 <- paste(atp2, paste0("'", names(atp2), "'"), sep = " = ")
            atp1 <- paste0(paste(atp1, collapse = ", "), "\n")
            atp2 <- paste(atp2, collapse = ", ")
            msg2 <- paste0(atp1, atp2)
        }
        msg3 <- c("\n...using default value.")
        warning(msg1, msg2, msg3, call. = FALSE)
    } else {
        return(atypes)
    }
}

#-------------------------------------------------------------------------------
.transform.linetype <- function(g) {
    if (ecount(g) > 0 && "edgeLineType" %in% names(edge_attr(g))) {
        lty <- E(g)$edgeLineType
        if (.all_integerValues(lty)) {
            ltypes <- .linetype.table()
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
        }
        E(g)$edgeLineType <- lty
    }
    return(g)
}
.linetype.table <- function() {
    atp <- c('blank' = 0, 'solid' = 1, 'dashed' = 2, 'dotted' = 3,
        'dotdash' = 4, 'longdash' = 5, 'twodash' = 6)
}


