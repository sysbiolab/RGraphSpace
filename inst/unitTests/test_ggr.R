# Unit tests fot GraphSpace
test_grs <- function(){
  data("gtoy1", package = "GraphSpace")
  gs <- GraphSpace(gtoy1)
  checkTrue(is(gs, "GraphSpace"))
}
