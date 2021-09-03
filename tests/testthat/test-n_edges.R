test_that("handles matrix", {
  x <- diag(16)
  expect_equal(n_edges(x), 16)
  expect_equal(n_edges(x, count_loops = FALSE), 0)
})

test_that("handles igraph", {
  x <- diag(16)
  g <- igraph::graph_from_adjacency_matrix(x)
  expect_equal(n_edges(g), 16)
  expect_equal(n_edges(g, count_loops = FALSE), 0)
})
