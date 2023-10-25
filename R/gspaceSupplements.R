
################################################################################
### Main constructor of GraphSpace-class objects
################################################################################
.buildGraphSpace <- function(g, mar = 0.075, verbose = TRUE) {
    if(!igraph::is.simple(g)){
        if(verbose && igraph::any_loop(g)) 
            message("Removing loops...")
        if(verbose && igraph::any_multiple(g)) 
            message("Merging duplicated edges...")
        g <- igraph::simplify(g, edge.attr.comb=list(weight="max", "first"))
    }
    # Get nodes and attributes
    vatt <- list("nodeLabel"=NA, "nodeLabelSize"=12, 
        "nodeLabelColor"="grey40", "nodeSize"=5, "nodeShape"=21,
        "nodeColor"="grey80", "nodeLineWidth"=1, "nodeLineColor"="grey20")
    if(verbose) message("Extracting vertices...")
    X <- igraph::V(g)$x
    Y <- igraph::V(g)$y
    vertex <- igraph::V(g)$name
    nodes <- data.frame(X = X, Y = Y, name=vertex)
    rownames(nodes) <- vertex
    nodes <- .center.nodes(nodes, mar)
    nodes <- .get.node.attrs(g, nodes, vatt)
    nodes$nodeSize <- nodes$nodeSize/100
    # Get edges and attributes
    eatt <- list("edgeLineWidth"=0.5, "edgeLineColor"="grey80", 
        "edgeLineType"="solid")
    aatt <- list("arrowLength"=1, "arrowAngle"=30)
    if(igraph::is.directed(g)){
        if(verbose) message("Extracting directed edges...")
        aatt$arrowType <- 1
        g <- .validate.eatt.dir(g, atts=c(eatt,aatt))
        edges <- .get.directed.edges(g, vertex, eatt, aatt)
    } else {
        if(verbose) message("Extracting undirected edges...")
        aatt$arrowType <- 0
        edges <- .get.undirected.edges(g, vertex, eatt, aatt)
    }
    edges <- .adjust.arrow.length(edges)
    if(verbose) message("Creating a 'GraphSpace' object...")
    pars <- list(mar = mar, is.directed = igraph::is.directed(g),
        default_att=list(vatt=vatt, eatt=eatt))
    grs <- new(Class = "GraphSpace", 
        nodes = nodes, edges = edges, pars = pars, 
        graph=g)
    return(grs)
}

#-------------------------------------------------------------------------------
.center.nodes <- function(nodes, mar){
    nodes$X <- nodes$X - mean(range(nodes$X))
    nodes$Y <- nodes$Y - mean(range(nodes$Y))
    from <- range(c(nodes$X, nodes$Y))
    to <- c(mar, 1-mar)
    nodes$X <- scales::rescale(nodes$X, from = from, to=to)
    nodes$Y <- scales::rescale(nodes$Y, from = from, to=to)
    return(nodes)
}

#-------------------------------------------------------------------------------
.get.node.attrs <- function(g, nodes, vatt){
    anames <- names(vatt)
    atts <- NULL
    bnames <- anames[anames %in% vertex_attr_names(g)]
    if(length(bnames)>0){
        atts <- vertex_attr(g)[bnames]
        .validate.vatt(atts)
    }
    bnames <- anames[ !anames%in%bnames ]
    if(length(bnames)>0){
        attout <- vatt[bnames]
        attout <- lapply(attout, function(lt) rep(lt, vcount(g)))
        atts <- c(atts, attout)
    }
    atts <- atts[anames]
    atts <- as.data.frame(atts)
    nodes <- cbind(nodes, atts)
    return(nodes)
}

#-------------------------------------------------------------------------------
.validate.vatt <- function(atts){
    if(!is.null(atts$nodeLabel)){
        .validate.args("allCharacterOrNa", "nodeLabel", atts$nodeLabel)
    }
    if(!is.null(atts$nodeLabelSize)){
        .validate.args("numeric_vec", "nodeLabelSize", atts$nodeLabelSize)
        if(min(atts$nodeLabelSize)<0){
            stop("'nodeLabelSize' should be a vector of numeric values >=0.")
        }
    }
    if(!is.null(atts$nodeLabelColor)){
        .validate.colors("allColors", "nodeLabelColor", atts$nodeLabelColor) 
    }
    if(!is.null(atts$nodeSize)){
        .validate.args("numeric_vec", "nodeSize", atts$nodeSize)
        if(max(atts$nodeSize)>100 || min(atts$nodeSize)<0){
            stop("'nodeSize' should be a vector of numeric values in [0, 100].")
        }
    }
    if(!is.null(atts$nodeShape)){
        .validate.args("integer_vec", "nodeShape", atts$nodeShape)
        if(max(atts$nodeShape)>25 || min(atts$nodeShape)<1){
            stop("'nodeShape' should be a vector of integer values in [0, 25].")
        }
    }
    if(!is.null(atts$nodeColor)){
        .validate.colors("allColors", "nodeColor", atts$nodeColor) 
    }
    if(!is.null(atts$nodeLineWidth)){
        .validate.args("numeric_vec", "nodeLineWidth", atts$nodeLineWidth)
        if(min(atts$nodeLineWidth)<0){
            stop("'nodeLineWidth' should be a vector of numeric values >=0.")
        }
    }
    if(!is.null(atts$nodeLineColor)){
        .validate.colors("allColors", "nodeLineColor", atts$nodeLineColor)
    }
}

#-------------------------------------------------------------------------------
.get.directed.edges <- function(g, vertex, eatt, aatt){
    E(g)$emode <- 1
    E(g)$emode[igraph::which_mutual(g)] <- 3
    e <- emode <- igraph::as_adjacency_matrix(g, sparse = FALSE, 
        attr = "emode")
    bl <- lower.tri(emode) & emode==3
    emode[bl] <- 0
    edges <- arrayInd(seq_len(prod(dim(emode))), dim(emode), 
        useNames = TRUE)
    edges <- as.data.frame(edges)
    colnames(edges) <- c("vertex1", "vertex2")
    edges$emode <- as.numeric(emode)
    edges$e <- as.numeric(e>0)
    eid <- e; eid[,] <- 0
    ut <- upper.tri(eid)
    eid[ut] <- 1:sum(ut)
    eid <- t(eid)
    eid[ut] <- 1:sum(ut)
    edges$eid <- as.numeric(eid)
    edges$ut <- as.numeric(upper.tri(e))
    edges$lt <- as.numeric(lower.tri(e))
    edges$name1 <- vertex[edges$vertex1]
    edges$name2 <- vertex[edges$vertex2]
    atts <- .get.eatt.dir(g, eaatt=c(eatt, aatt))
    if(!all(atts[,c(1,2)]==edges[,c(1,2)])){
        stop("unexpected indexing during edge attribute combination.")
    }
    edges <- cbind(edges, atts[,-c(1,2)])
    eid <- unique(edges$eid[edges$e>0])
    edges <- edges[edges$eid%in%eid,]
    edges <- edges[order(edges$eid), ]
    rownames(edges) <- NULL
    edges <- .set.arrowtype.dir(edges, aatt)
    edges <- .set.emode.dir(edges)
    return(edges)
}
.validate.eatt.dir <- function(g, atts){
    a_names <- names(atts)
    b_names <- a_names[a_names %in% edge_attr_names(g)]
    .validate.eatt(edge_attr(g)[b_names])
    c_names <- a_names[ !a_names%in%b_names ]
    if(length(c_names)>0){
        for(at in c_names){
            edge_attr(g, name=at) <- atts[[at]]
        }
    }
    return(g)
}
.get.eatt.dir <- function(g, eaatt){
    e <- igraph::as_adjacency_matrix(g, sparse=FALSE)
    atts <- arrayInd(seq_len(prod(dim(e))), dim(e), useNames = TRUE)
    atts <- as.data.frame(atts)
    colnames(atts) <- c("vertex1", "vertex2")
    atts$e <- as.numeric(e)
    a_names <- names(eaatt)
    if(!all(a_names %in% edge_attr_names(g))){
        stop("Olha isso!")
    }
    ne <- e==0
    g <- .transform.arrowType(g)
    for(at in a_names){
        x <- igraph::as_adjacency_matrix(g, sparse=FALSE, attr = at)
        x[ne] <- NA
        if(is.numeric(x)){
            atts[[at]] <- as.numeric(x)
        } else {
            atts[[at]] <- as.character(x)
        }
    }
    rownames(atts) <- NULL
    atts <- atts[,c("vertex1", "vertex2", a_names)]
    return(atts)
}
.set.arrowtype.dir <- function(edges, aatt){
    # Flip ut/lt from single-edge arrows; this
    # for collecting arrows from the same mtx side
    idx <- which(edges$emode==1 & edges$lt==1)
    if(length(idx)>0){
        for(i in idx){
            ii <- which(edges$eid==edges$eid[i])
            edges[ii, c("ut","lt")] <- edges[ii, c("lt","ut")]
        }
    }
    # collect left-side arrows
    nms <- names(aatt)
    arrow1 <- edges[edges$lt==1, nms]
    arrow1[is.na(arrow1)] <- 0
    arrow1 <- .set.arrowangle.dir(arrow1)
    colnames(arrow1) <- paste0(nms, "_",1)
    # collect right-side arrows
    arrow2 <- edges[edges$ut==1, nms]
    arrow2[is.na(arrow2)] <- 0
    arrow2 <- .set.arrowangle.dir(arrow2)
    colnames(arrow2) <- paste0(nms, "_",2)
    # get single-edge assigments
    edges <- edges[ , -which(colnames(edges) %in% nms)]
    edges <- edges[edges$e==1, ]
    eid <- sort(unique(edges$eid))
    edges <- edges[order(-edges$ut, edges$eid),]
    edges <- edges[match(eid, edges$eid),]
    # add arrows and remove intermediate columns
    edges <- cbind(edges, arrow1, arrow2)
    edges <- edges[, -which(colnames(edges) %in% 
            c("e","eid","ut","lt"))]
    return(edges)
}
.set.arrowangle.dir <- function(atts){
    atts$arrowAngle[atts$arrowType ==  0] <- 0
    atts$arrowAngle[atts$arrowType ==  1] <- 30
    atts$arrowAngle[atts$arrowType == -1] <- 90
    return(atts)
}
.set.emode.dir <- function(edges){
    emode <- (abs(edges$arrowType_1) * 2) + abs(edges$arrowType_2)
    edges$emode <- emode
    return(edges)
}

#-------------------------------------------------------------------------------
.get.undirected.edges <- function(g, vertex, eatt, aatt){
    edges <- igraph::as_edgelist(g, names = FALSE)
    rownames(edges) <- colnames(edges) <- NULL
    edges <- as.data.frame(edges)
    colnames(edges) <- c("vertex1", "vertex2")
    edges$emode <- 0
    edges$name1 <- vertex[edges$vertex1]
    edges$name2 <- vertex[edges$vertex2]
    atts <- .get.eatt.und(g, c(eatt, aatt))
    if(!all(atts[,c(1,2)]==edges[,c(1,2)])){
        stop("unexpected indexing during edge attribute combination.")
    }
    edges <- cbind(edges, atts[,-c(1,2)])
    edges <- edges[order(edges$vertex1,edges$vertex2), ]
    edges <- .set.arrowtype.und(edges, aatt)
    edges <- .set.emode.und(edges)
    return(edges)
}
.get.eatt.und <- function(g, eatt){
    a_names <- names(eatt)
    atts <- NULL
    b_names <- a_names[a_names %in% edge_attr_names(g)]
    if(length(b_names)>0){
        atts <- igraph::edge_attr(g)[b_names]
        .validate.eatt(atts)
    }
    c_names <- a_names[ !a_names%in%b_names ]
    if(length(c_names)>0){
        attout <- eatt[c_names]
        attout <- lapply(attout, function(lt) rep(lt, ecount(g)))
        atts <- c(atts, attout)
    }
    atts <- atts[a_names]
    atts <- as.data.frame(atts)
    e <- igraph::as_edgelist(g, names = FALSE)
    colnames(e) <- c("vertex1", "vertex2")
    atts <- cbind(e, atts)
    return(atts)
}
.set.arrowtype.und <- function(edges, aatt){
    a_names <- names(aatt)
    a_names <- a_names[-which(a_names=="arrowType")]
    arrow1 <- arrow2 <- edges[,a_names]
    arrow1 <- .set.arrowangle1.und(arrow1, edges$arrowType)
    arrow2 <- .set.arrowangle2.und(arrow2, edges$arrowType)
    edges <- edges[, -which(colnames(edges)%in%a_names)]
    edges <- cbind(edges, arrow1, arrow2)
    return(edges)
}
.set.arrowangle1.und <- function(arrow1, etype){
    arrow1$arrowAngle[etype %in% c(0, 1, -1)] <- 0
    arrow1$arrowAngle[etype %in% c(2, 3, -4)] <- 30
    arrow1$arrowAngle[etype %in% c(-2, -3, 4)] <- 90
    colnames(arrow1) <- paste0(names(arrow1), "_", 1)
    return(arrow1)
}
.set.arrowangle2.und <- function(arrow2, etype){
    arrow2$arrowAngle[etype %in% c(0, 2, -2)] <- 0
    arrow2$arrowAngle[etype %in% c(1, 3, 4)] <- 30
    arrow2$arrowAngle[etype %in% c(-1, -3, -4)] <- 90
    colnames(arrow2) <- paste0(names(arrow2), "_", 2)
    return(arrow2)
}
.set.emode.und <- function(edges){
    emode <- abs(edges$arrowType)
    emode[emode>3] <- 3
    edges$emode <- emode
    return(edges)
}
.adjust.arrow.length <- function(edges){
    a_theta <- 60 #default arrow angle * 2 (not implemented)
    a_theta <- a_theta / 180 * pi
    idx <- edges$arrowAngle_1==90
    if(any(idx, na.rm = TRUE)){
        l <- edges$arrowLength_1[idx]/2
        b <- sqrt( (l^2 + l^2) - (2 * l^2) * cos(a_theta))
        edges$arrowLength_1[idx] <- b + edges$edgeLineWidth[idx]/4
    }
    idx <- edges$arrowAngle_2==90
    if(any(idx, na.rm = TRUE)){
        l <- edges$arrowLength_2[idx]/2
        b <- sqrt( (l^2 + l^2) - (2 * l^2) * cos(a_theta))
        edges$arrowLength_2[idx] <- b + edges$edgeLineWidth[idx]/4
    }
    return(edges)
}

#-------------------------------------------------------------------------------
.validate.eatt <- function(atts){
    if(!is.null(atts$edgeLineWidth)){
        .validate.args("numeric_vec", "edgeLineWidth", atts$edgeLineWidth)
        if(min(atts$edgeLineWidth)<0){
            stop("'edgeLineWidth' should be a vector of numeric values >=0.")
        }
    }
    if(!is.null(atts$edgeLineColor)){
        .validate.colors("allColors", "edgeLineColor", atts$edgeLineColor)
    }
    if(!is.null(atts$edgeLineType)){
        .validate.args("allCharacterOrInteger", "edgeLineType", 
            atts$edgeLineType)
    }
    if(!is.null(atts$arrowLength)){
        .validate.args("numeric_vec", "arrowLength", atts$arrowLength)
        if(min(atts$arrowLength)<0){
            stop("'arrowLength' should be a vector of numeric values >=0.")
        }
    }
    if(!is.null(atts$arrowType)){
        .validate.args("allCharacterOrInteger", "arrowType", atts$arrowType)
    }
}
.transform.arrowType <- function(g){
    if("arrowType" %in% names(edge_attr(g))){
        eatt <- E(g)$arrowType
        gtype <- is.directed(g)
        aty <- .get.arrowTypes(gtype)
        if(.all_integerValues(eatt)){
            idx <- !eatt %in% aty
            if(any(idx)){
                eatt[idx] <- 1
                .get.arrowTypes(gtype, TRUE)
            }
        } else {
            idx <- !eatt %in% names(aty)
            if(any(idx)){
                eatt[idx] <- "-->"
                .get.arrowTypes(gtype, TRUE)
            }
            eatt <- aty[eatt]
        }
        
        E(g)$arrowType <- eatt
    }
    return(g)
}
.get.arrowTypes <- function(is.dir=FALSE, get.msg=FALSE){
    atp1 <- c(
        "---"=0,"--"=0, "-"=0,
        "-->"=1, "->"=1, ">"=1,
        "<--"=2, "<-"=2, "<"=2,
        "<->"=3, "<>"=3,  
        "|->"=4, "|>"=4)
    atp2 <- c(
        "--|"=-1, "-|"=-1, "|"=-1,
        "|--"=-2, "|-"=-2, "|"=-2,
        "|-|"=-3, "||"=-3, 
        "<-|"=-4, "<|"=-4)
    atypes <- c(atp1,atp2)
    if(is.dir){
        atypes <- atypes[atypes %in% c(-1, 0, 1)]
    }
    if(get.msg){
        gtype <- ifelse(is.dir, "directed", "undirected")
        msg1 <- paste("'arrowType' integer or character code in",
            gtype, "graphs should be in:\n")
        if(is.dir){
            msg2 <- atypes[match(unique(atypes), atypes)]
            msg2 <- paste(msg2, paste0("'",names(msg2),"'"), sep = " = ")
            msg2 <- paste(msg2, collapse = ", ")
        } else {
            atp1 <- atp1[match(unique(atp1), atp1)]
            atp2 <- atp2[match(unique(atp2), atp2)]
            atp1 <- paste(atp1, paste0("'",names(atp1),"'"), sep = " = ")
            atp2 <- paste(atp2, paste0("'",names(atp2),"'"), sep = " = ")
            atp1 <- paste0(paste(atp1, collapse = ", "),"\n")
            atp2 <- paste(atp2, collapse = ", ")
            msg2 <- paste0(atp1,atp2)
        }
        warning(msg1, msg2, call. = FALSE)
    } else {
        return(atypes)
    }
}
