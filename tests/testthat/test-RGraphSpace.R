# Tests for RGraphSpace-class methods
test_that("Check RGraphSpace-class methods", {
  data("gtoy1", package = "RGraphSpace")
  gs <- GraphSpace(gtoy1)
  expect_true(is(gs, "GraphSpace"))
})
