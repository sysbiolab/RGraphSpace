# Unit tests fot GraphSpace
test_grs <- function(){
  data("gtoy1", package = "GraphSpace")
  checkTrue(is(gtoy1, "igraph"))
}
