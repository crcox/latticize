test_that("handles matrix", {
  x <- diag(2)
  expect_equal(vertices2edge(x, matrix(c(2, 2), nrow = 2)), 4)
})

test_that("handles igraph", {
  x <- diag(2)
  g <- igraph::graph_from_adjacency_matrix(x)
  expect_equal(vertices2edge(g, matrix(c(2, 2), nrow = 2)), 2)
})
