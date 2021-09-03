test_that("handles matrix", {
  x <- matrix(1:9, nrow = 3)
  edges <- cbind(
    c(1, 2),
    c(1, 3),
    c(3, 2)
  )
  expect_equal(get_weights(x, edges), c(4, 7, 6))
})

test_that("handles matrix", {
  x <- matrix(1:9, nrow = 3)
  g_unweighted <- igraph::graph_from_adjacency_matrix(x, weighted = NULL)
  g_weighted <- igraph::graph_from_adjacency_matrix(x, weighted = TRUE)
  edges <- cbind(
    c(1, 2),
    c(1, 3),
    c(3, 2)
  )
  expect_equal(get_weights(g_unweighted, edges), c(1, 1, 1))
  expect_equal(get_weights(g_weighted, edges), c(4, 7, 6))
})
