test_that("handles matrix", {
  x <- matrix(1:9, nrow = 3)
  expect_equal(n_vertices(x), 3)
})

test_that("handles matrix", {
  x <- matrix(1:9, nrow = 3)
  g <- igraph::graph_from_adjacency_matrix(x)
  expect_equal(n_vertices(g), 3)
})
