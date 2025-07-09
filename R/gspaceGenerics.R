
setGeneric("plotGraphSpace", function(gs, ...) 
  standardGeneric("plotGraphSpace"),
  package = "RGraphSpace"
)

setGeneric("getGraphSpace", function(gs, what = "summary") 
  standardGeneric("getGraphSpace"), 
  package = "RGraphSpace"
)

setGeneric("gs_vcount", function(x) 
  standardGeneric("gs_vcount"),
  package = "RGraphSpace"
)

setGeneric("gs_ecount", function(x) 
  standardGeneric("gs_ecount"),
  package = "RGraphSpace"
)

setGeneric("gs_vertex_attr", function(x, ...) 
  standardGeneric("gs_vertex_attr"),
  package = "RGraphSpace"
)

setGeneric("gs_vertex_attr<-", function(x, ..., value) 
  standardGeneric("gs_vertex_attr<-"),
  package = "RGraphSpace"
)

setGeneric("gs_edge_attr", function(x, ...) 
  standardGeneric("gs_edge_attr"),
  package = "RGraphSpace"
)

setGeneric("gs_edge_attr<-", function(x, ..., value) 
  standardGeneric("gs_edge_attr<-"),
  package = "RGraphSpace"
)
