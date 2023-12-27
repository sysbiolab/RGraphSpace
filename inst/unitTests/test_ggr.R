# Unit tests fot RGraphSpace
test_grs <- function(){
  data("gtoy1", package = "RGraphSpace")
  gs <- GraphSpace(gtoy1)
  checkTrue(is(gs, "GraphSpace"))
}
