
setGeneric("GraphSpace", function(g, ...) 
  standardGeneric("GraphSpace"),
  package = "RGraphSpace"
)

setGeneric("plotGraphSpace", function(gs, ...)
  standardGeneric("plotGraphSpace"),
  package = "RGraphSpace"
)

setGeneric("getGraphSpace", function(gs, ...) 
  standardGeneric("getGraphSpace"), 
  package = "RGraphSpace"
)

setGeneric("normalizeGraphSpace", function(gs, ...) 
  standardGeneric("normalizeGraphSpace"),
  package = "RGraphSpace"
)

setGeneric("cropGraphSpace", function(gs, ...) 
  standardGeneric("cropGraphSpace"),
  package = "RGraphSpace"
)

setGeneric("rotateGraphSpace", function(gs, ...)
  standardGeneric("rotateGraphSpace"),
  package = "RGraphSpace"
)

setGeneric("flipGraphSpace", function(gs, ...)
  standardGeneric("flipGraphSpace"),
  package = "RGraphSpace"
)

setGeneric("transposeGraphSpace", function(gs, ...)
  standardGeneric("transposeGraphSpace"),
  package = "RGraphSpace"
)

setGeneric("normalizeGeometry", function(gs, ...)
  standardGeneric("normalizeGeometry"),
  package = "RGraphSpace"
)

setGeneric("fitGeometry", function(gs, ...)
  standardGeneric("fitGeometry"),
  package = "RGraphSpace"
)

setGeneric("gs_delete_v_attr", function(x, name)
  standardGeneric("gs_delete_v_attr"),
  package = "RGraphSpace"
)

setGeneric("gs_delete_e_attr", function(x, name)
  standardGeneric("gs_delete_e_attr"),
  package = "RGraphSpace"
)

setGeneric("gs_graph", function(x) 
  standardGeneric("gs_graph"),
  package = "RGraphSpace"
)

setGeneric("gs_names", function(x) 
  standardGeneric("gs_names"),
  package = "RGraphSpace"
)

setGeneric("gs_scale_factor", function(x) 
  standardGeneric("gs_scale_factor"),
  package = "RGraphSpace"
)

setGeneric("gs_scale_factor<-", function(x, value) 
  standardGeneric("gs_scale_factor<-"),
  package = "RGraphSpace"
)

setGeneric("gs_nodes", function(x, ...) 
  standardGeneric("gs_nodes"),
  package = "RGraphSpace"
)

setGeneric("gs_edges", function(x, ...) 
  standardGeneric("gs_edges"),
  package = "RGraphSpace"
)

setGeneric("gs_image", function(x) 
  standardGeneric("gs_image"),
  package = "RGraphSpace"
)

setGeneric("gs_image<-", function(x, value) 
  standardGeneric("gs_image<-"),
  package = "RGraphSpace"
)

setGeneric("gs_image_maxpixels", function(x) 
  standardGeneric("gs_image_maxpixels"),
  package = "RGraphSpace"
)

setGeneric("gs_image_maxpixels<-", function(x, value) 
  standardGeneric("gs_image_maxpixels<-"),
  package = "RGraphSpace"
)

setGeneric("gs_add_edges",  function(x, value, ...) 
  standardGeneric("gs_add_edges"),  
  package = "RGraphSpace"
)

setGeneric("gs_add_edges<-", function(x, value)
  standardGeneric("gs_add_edges<-"),
  package = "RGraphSpace"
)

setGeneric("gs_add_nodes",  function(x, value, ...) 
  standardGeneric("gs_add_nodes"),  
  package = "RGraphSpace"
)

setGeneric("gs_add_nodes<-", function(x, value)
  standardGeneric("gs_add_nodes<-"),
  package = "RGraphSpace"
)

setGeneric("gs_fdata", function(x) 
  standardGeneric("gs_fdata"),
  package = "RGraphSpace"
)

setGeneric("gs_fdata<-", function(x, value)
  standardGeneric("gs_fdata<-"),
  package = "RGraphSpace"
)

setGeneric("gs_nfeatures", function(x) 
  standardGeneric("gs_nfeatures"),
  package = "RGraphSpace"
)

setGeneric("gs_features", function(x) 
  standardGeneric("gs_features"),
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

setGeneric("gs_geometry", function(x, ...) 
  standardGeneric("gs_geometry"),
  package = "RGraphSpace"
)

setGeneric("gs_geometry<-", function(x, ..., value) 
  standardGeneric("gs_geometry<-"),
  package = "RGraphSpace"
)
