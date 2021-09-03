test_that("handles matrix", {
  x <- rbind(c(0, 0), c(0, 1))
  expect_true(edge_exists(x, as.matrix(c(2, 2))))
  expect_false(edge_exists(x, as.matrix(c(1, 2))))
})

test_that("handles igraph", {
  x <- igraph::graph_from_adjacency_matrix(rbind(c(0, 0), c(0, 1)))
  expect_true(edge_exists(x, as.matrix(c(2, 2))))
  expect_false(edge_exists(x, as.matrix(c(1, 2))))
})
