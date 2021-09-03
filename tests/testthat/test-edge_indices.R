test_that("returns accurate indices for matrix input", {
  x <- diag(4)
  expect_equal(edge_indices(x), c(1, 6, 11, 16))
})
