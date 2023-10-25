
setGeneric("plotGraphSpace", 
    function(grs, xlab = "Graph coordinates 1", ylab = "Graph coordinates 2",
        font.size = 1, theme.name = c("th1", "th2", "th3"),
        bg.color = "grey95", marks = FALSE, mark.size = 3, 
        mark.color = "grey20") standardGeneric("plotGraphSpace"),
    signature = "grs"
)

setGeneric("getGraphSpace", 
    function(grs, what = "summary") standardGeneric("getGraphSpace"),
    package = "GraphSpace"
)

setGeneric("igraphSpace", 
    function(g, ..., layout = NULL, mar = 0.075) standardGeneric("igraphSpace"),
    signature = "g"
)
