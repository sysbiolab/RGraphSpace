
#-------------------------------------------------------------------------------
#' @title Plotting igraph objects with GraphSpace
#'
#' @description \code{igraphSpace} is a wrapper function for 
#' GraphSpace plotting calls
#' 
#' @param g An \code{igraph} object. It must include \code{x}, \code{y}, 
#' and \code{name}  vertex attributes (see \code{\link{GraphSpace}}).
#' @param ... Additional arguments passed to the 
#' \code{\link{plotGraphSpace}} function.
#' @param layout an optional numeric matrix with two columns for \code{x} 
#' and \code{y} coordinates.
#' @param mar A single numeric value (in \code{[0,1]}) indicating the size of
#' the outer margins as a fraction of the graph space.
#' @return A ggplot-class object.
#' @author Sysbiolab.
#' @seealso \code{\link{plotGraphSpace}}
#' @examples
#' # Load a demo igraph
#' data('gtoy1', package = 'GraphSpace')
#' 
#' # Generate a ggplot for the gtoy1 igraph
#' igraphSpace(gtoy1)
#'
#' @import methods
#' @docType methods
#' @rdname igraphSpace-methods
#' @aliases igraphSpace
#' @export
#'
setMethod("igraphSpace", "igraph", 
    function(g, ..., layout = NULL, mar = 0.075) {
        grs <- GraphSpace(g, layout, mar, verbose=FALSE)
        gg <- plotGraphSpace(grs, ...=...)
        return(gg)
    }
)


#-------------------------------------------------------------------------------
#' @title Plotting graphs with the GraphSpace package
#'
#' @description \code{plotGraphSpace} is a wrapper function to 
#' create dedicated ggplot graphics for GraphSpace-class objects.
#'
#' @param grs A \linkS4class{GraphSpace} class object.
#' @param xlab The title for the 'x' axis of a 2D-image space.
#' @param ylab The title for the 'y' axis of a 2D-image space.
#' @param font.size A single numeric value passed to ggplot themes.
#' @param theme.name Name of a custom GraphSpace theme. These themes 
#' (from 'th1' to 'th3') consist mainly of preconfigured ggplot settings, 
#' which the user can subsequently fine-tune within the resulting 
#' ggplot object.
#' @param bg.color A single color for background.
#' @param marks A logical value indicating whether to add 'marks' to vertex 
#' positions. Alternatively, this could be a vector listing vertex names.
#' @param mark.size A font size argument passed to \code{\link{geom_text}}.
#' @param mark.color A color passed to \code{\link{geom_text}}.
#' @return A ggplot-class object.
#' @author Sysbiolab.
#' @seealso \code{\link{GraphSpace}}
#' @examples
#' # Load a demo igraph
#' data('gtoy1', package = 'GraphSpace')
#' 
#' grs <- GraphSpace(gtoy1)
#' 
#' plotGraphSpace(grs)
#' 
#' @import methods
#' @importFrom ggplot2 geom_point geom_segment aes Geom .pt .stroke
#' @importFrom ggplot2 element_rect margin element_blank layer
#' @importFrom ggplot2 element_line element_text ggproto
#' @importFrom grDevices col2rgb
#' @importFrom grid gpar arrow unit pointsGrob polygonGrob
#' @importFrom scales alpha
#' @docType methods
#' @rdname plotGraphSpace-methods
#' @aliases plotGraphSpace
#' @export
#'
setMethod("plotGraphSpace", "GraphSpace", 
    function(grs, xlab = "Graph coordinates 1", ylab = "Graph coordinates 2",
        font.size = 1, theme.name = c("th1", "th2", "th3"),
        bg.color = "grey95", marks = FALSE, mark.size = 3, 
        mark.color = "grey20") {
        #--- validate the grs object and args
        .validate.args("singleString", "xlab", xlab)
        .validate.args("singleString", "ylab", ylab)
        .validate.args("singleNumber", "font.size", font.size)
        theme.name <- match.arg(theme.name)
        .validate.colors("singleColor", "bg.color", bg.color)
        .validate.plot.args("marks", marks)
        .validate.args("numeric_vec","mark.size", mark.size)
        .validate.colors("singleColor","mark.color", mark.color)
        #--- get slots from grs
        nodes <- getGraphSpace(grs, "nodes")
        edges <- getGraphSpace(grs, "edges")
        pars <- getGraphSpace(grs, "pars")
        #--- get edge coordinates
        edges <- .get.exy(nodes, edges)
        #--- set theme pars
        cl <- .set.theme.bks(theme.name)
        #--- get ggplot object
        ggp <- .get.graph(nodes, edges, xlab, ylab, cl)
        if(pars$is.directed){
            ggp <- .set.dir.graph(ggp, nodes, edges)
        } else {
            ggp <- .set.und.graph(ggp, nodes, edges)
        }
        #--- add marks if available
        bl <- is.logical(marks) && marks
        if (bl || is.character(marks)) {
            if(bl) marks <- rownames(nodes)
            ggp <- .add.node.marks(ggp, nodes, marks, mark.color, 
                mark.size)
        }
        #--- apply custom theme
        ggp <- .custom.themes(ggp, theme.name, 
            font.size=font.size, bg.color=bg.color)
        return(ggp)
    }
)

#-------------------------------------------------------------------------------
.get.exy <- function(gxy, edges){
    exy <- data.frame(
        x1 = gxy[edges$vertex1,"X"], 
        x2 = gxy[edges$vertex2,"X"], 
        y1 = gxy[edges$vertex1,"Y"], 
        y2 = gxy[edges$vertex2,"Y"])
    edges <- cbind(edges, exy)
    return(edges)
}

#-------------------------------------------------------------------------------
.get.graph <- function(nodes, edges, xlab, ylab, cl){
    X <- Y <- NULL
    ggp <- ggplot2::ggplot(nodes, ggplot2::aes(X, Y)) +
        ggplot2::scale_x_continuous(name = xlab, breaks = cl$axis.ticks,
            labels = format(cl$axis.ticks), position = cl$x.position,
            limits = cl$xylim, expand = ggplot2::expansion(mult = 0)) +
        ggplot2::scale_y_continuous(name = ylab, breaks = cl$axis.ticks,
            labels = format(cl$axis.ticks), limits = cl$xylim,
            expand = ggplot2::expansion(mult = 0)) +
        ggplot2::coord_fixed()
    return(ggp)
}

#-------------------------------------------------------------------------------
.set.und.graph <- function(ggp, nodes, edges){
    edges <- .offset.edges(nodes, edges)
    ggp <- .add.segments(ggp, edges)
    ggp <- .add.arrows(ggp, edges)
    ggp <- .add.nodes(ggp, nodes)
    return(ggp)
}

#-------------------------------------------------------------------------------
.set.dir.graph <- function(ggp, nodes, edges){
    edges <- .offset.edges(nodes, edges)
    ggp <- .add.segments(ggp, edges)
    ggp <- .add.arrows(ggp, edges)
    ggp <- .add.nodes(ggp, nodes)
    return(ggp)
}

#-------------------------------------------------------------------------------
.add.nodes <- function(ggp, nodes){
    ggp <- ggp + .geom_nodespace(size = grid::unit(nodes$nodeSize, "npc"), 
        fill = nodes$nodeColor, colour=nodes$nodeLineColor, 
        shape=nodes$nodeShape, stroke = nodes$nodeLineWidth)
    return(ggp)
}
.add.segments <- function(ggp, edges){
    x1 <- y1 <- x2 <- y2 <- NULL
    ggp <- ggp + ggplot2::geom_segment(
        aes(x = x1, y = y1, xend = x2, yend = y2), 
        data = edges, linewidth = edges$edgeLineWidth, 
        linetype = edges$edgeLineType, linejoin = "mitre",  
        colour = edges$edgeLineColor, show.legend = FALSE)
    return(ggp)
}
.add.arrows <- function(ggp, edges){
    x1 <- y1 <- x2 <- y2 <- NULL
    #--- add arrow ends
    idx <- edges$emode==1 | edges$emode==3
    if(sum(idx)>0){
        edges2 <- edges[idx,]
        ends <-.get.arrow.ends(edges2)
        ggp <- ggp + ggplot2::geom_segment(
            aes(x = x1, y = y1, xend = x2, yend = y2), 
            data = ends$exy, linewidth = edges2$edgeLineWidth, 
            linetype = 1, linejoin = "mitre", colour = edges2$edgeLineColor, 
            arrow = ends$arrow, show.legend = FALSE)
    }
    #--- add arrow starts
    idx <- edges$emode==2 | edges$emode==3
    if(sum(idx)>0){
        edges2 <- edges[idx,]
        starts <-.get.arrow.starts(edges2)
        ggp <- ggp + ggplot2::geom_segment(
            aes(x = x1, y = y1, xend = x2, yend = y2), 
            data = starts$exy, linewidth = edges2$edgeLineWidth, 
            linetype = 1, linejoin = "mitre", colour = edges2$edgeLineColor, 
            arrow = starts$arrow, show.legend = FALSE)
    }
    return(ggp)
}

#-------------------------------------------------------------------------------
.offset.edges <- function(nodes, edges){
    offset <- nodes$nodeSize/2 + 0.02
    edges$offset.start <- ifelse(edges$emode %in% c(0,1), 0, 
        offset[edges$vertex1])
    edges$offset.end <- ifelse(edges$emode %in% c(0,2), 0, 
        offset[edges$vertex2])
    edges <- .offset.exy(edges)
    return(edges)
}
.offset.exy <- function(edges){
    dx <- edges$x2 - edges$x1
    dy <- edges$y2 - edges$y1
    dist <- sqrt( dx^2 + dy^2 )
    edges$px <- dx/dist
    edges$py <- dy/dist
    edges$x1 <- edges$x1 + (edges$px * edges$offset.start)
    edges$y1 <- edges$y1 + (edges$py * edges$offset.start)
    edges$x2 <- edges$x2 - (edges$px * edges$offset.end)
    edges$y2 <- edges$y2 - (edges$py * edges$offset.end)
    return(edges)
}

#-------------------------------------------------------------------------------
.get.arrow.ends <- function(edges){
    exy <- edges[,c("x1", "y1", "x2", "y2")]
    exy$x1 <- exy$x2 - (edges$px * 0.01)
    exy$y1 <- exy$y2 - (edges$py * 0.01)
    arrow <- grid::arrow(angle = edges$arrowAngle_2,
        type = "open", ends = "last",
        length = grid::unit(edges$arrowLength_2 * ggplot2::.pt, "mm"))
    return(list(exy=exy, arrow=arrow))
}
.get.arrow.starts <- function(edges){
    exy <- edges[,c("x1", "y1", "x2", "y2")]
    exy$x2 <- exy$x1 + (edges$px * 0.01)
    exy$y2 <- exy$y1 + (edges$py * 0.01)
    arrow <- grid::arrow(angle = edges$arrowAngle_1,
        type = "open", ends = "first",
        length = grid::unit(edges$arrowLength_1 * ggplot2::.pt, "mm"))
    return(list(exy=exy, arrow=arrow))
}

#-------------------------------------------------------------------------------
GeomNodeSpace <- ggproto("GeomNodeSpace", Geom,
    required_aes = c("x", "y", "size"),
    default_aes = aes(shape = 21, size = grid::unit(0.1, "npc"), 
        fill = "grey90", colour = "grey20", stroke = 1, alpha = NA),
    draw_panel = function(data, panel_scales, coord) {
        coords <- coord$transform(data, panel_scales)
        grid::pointsGrob(
            x = coords$x,
            y = coords$y,
            pch = coords$shape,
            # for pch in 0:25, size is about 75% of the
            # character height (see 'points()' graphics)
            size = coords$size * 1.25,
            default.units = "npc",
            gp = grid::gpar(
                fill = scales::alpha(coords$fill, coords$alpha), 
                col = scales::alpha(coords$colour, coords$alpha),
                lwd = coords$stroke
            )
        )
    }
)
.geom_nodespace <- function(mapping = NULL, data = NULL, 
    stat = "identity", position = "identity", na.rm = TRUE, 
    show.legend = FALSE, inherit.aes = TRUE, ...) {
    ggplot2::layer(
        geom = GeomNodeSpace, mapping = mapping,  
        data = data, stat = stat, position = position, 
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

#-------------------------------------------------------------------------------
.add.node.marks <- function(ggp, nodes, marks, mark.color, 
    mark.size) {
    gxy <- nodes
    if (is.null(names(marks))) names(marks) <- marks
    names(marks) <- ifelse(names(marks) == "", marks, names(marks))
    marks <- marks[marks %in% rownames(gxy)]
    if (length(mark.color) > 1) {
        if (is.null(names(mark.color))) {
            stop("'mark.color' should be named for this call!", call. = FALSE)
        }
        if (!all(marks %in% names(mark.color))) {
            stop("All 'marks' should be listed in 'mark.color' names!",
                call. = FALSE)
        }
        mark.color <- mark.color[marks]
    }
    if (length(mark.size) > 1) {
        if (is.null(names(mark.size))) {
            stop("'mark.size' should be named for this call!", call. = FALSE)
        }
        if (!all(marks %in% names(mark.size))) {
            stop("All 'marks' should be listed in 'mark.size' names!",
                call. = FALSE)
        }
        mark.size <- mark.size[marks]
    }
    gxy$mark.color <- mark.color
    gxy$mark.size <- mark.size
    gxy <- gxy[marks, , drop = FALSE]
    gxy$ID <- names(marks); ID <- NULL
    ggp <- ggp + ggplot2::geom_text(mapping = aes(label = ID), 
        data = gxy, fontface = "bold",
        size = mark.size, colour = mark.color, 
        vjust="center", hjust="center")
    return(ggp)
}

#-------------------------------------------------------------------------------
.custom.themes <- function(gg, theme.name, font.size, bg.color) {
    et1 <- ggplot2::element_text(size = 14 * font.size)
    et2 <- ggplot2::element_text(size = 12 * font.size)
    if (theme.name == "th3") {
        gg <- .custom.th3(gg, font.size, bg.color)
    } else if (theme.name == "th2") {
        gg <- .custom.th2(gg, font.size, bg.color)
    } else {
        gg <- .custom.th1(gg, font.size, bg.color)
    }
    return(gg)
}
.custom.th1 <- function(gg, font.size, bg.color) {
    et1 <- ggplot2::element_text(size = 14 * font.size)
    et2 <- ggplot2::element_text(size = 12 * font.size)
    gg <- gg + ggplot2::theme(axis.title = et1, axis.text = et2,
        legend.title = et2, legend.text = et2,
        panel.background = element_rect(fill = bg.color))
    return(gg)
}
.custom.th2 <- function(gg, font.size, bg.color) {
    et1 <- ggplot2::element_text(size = 14 * font.size)
    et2 <- ggplot2::element_text(size = 12 * font.size)
    gg <- gg + ggplot2::theme_gray() + ggplot2::theme(axis.title = et1,
        axis.text = et2, legend.title = et2,
        legend.text = et2, legend.margin = margin(0, 0, 0, 0), 
        plot.margin = margin(5, 10, 0, 10), 
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        plot.background = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_line(linewidth = 0.7),
        axis.line = element_blank(), panel.border = element_blank(),
        panel.background = element_rect(fill = bg.color))
    return(gg)
}
.custom.th3 <- function(gg, font.size, bg.color) {
    et1 <- ggplot2::element_text(size = 14 * font.size)
    et2 <- ggplot2::element_text(size = 12 * font.size)
    gg <- gg + ggplot2::theme_gray() + ggplot2::theme(axis.title = et1,
        axis.text = et2, legend.title = element_text(size = 12 * 
                font.size, vjust = 1), legend.text = et2,
        legend.margin = margin(0, 0, 0, 0),
        legend.position = "bottom", plot.margin = margin(5, 5, 5, 5), 
        legend.box.margin = margin(0, 0, 0, 0), 
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        plot.background = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_line(linewidth = 0.5),
        axis.line = element_blank(), panel.border = element_blank(),
        panel.background = element_rect(fill = bg.color))
    return(gg)
}
.set.theme.bks <- function(theme.name, cl=list()){
    if (theme.name %in% c("th3")) {
        cl$axis.ticks <- c(0.25, 0.5, 0.75)
        cl$xylim <- c(-0.01, 1.01)
        cl$x.position <- "top"
        cl$justify <- "centre"
    } else if (theme.name %in% c("th2")) {
        cl$axis.ticks <- seq(0.1, 0.9, 0.2)
        cl$xylim <- c(-0.01, 1.01)
        cl$x.position <- "bottom"
        cl$justify <- "right"
    } else {
        cl$axis.ticks <- seq(0, 1, 0.2)
        cl$xylim <- c(-0.05, 1.05)
        cl$x.position <- "bottom"
        cl$justify <- "right"
    }
    return(cl)
}
