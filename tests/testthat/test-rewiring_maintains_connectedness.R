test_that("positive case", {
  x <- rbind(
    c(0, 1, 0, 1),
    c(1, 0, 1, 0),
    c(1, 0, 0, 0),
    c(0, 1, 1, 0)
  )
  g <- igraph::graph_from_adjacency_matrix(x)
  # r <- rbind(
  #   c(0, 1, 0, 1),
  #   c(1, 0, 1, 0),
  #   c(0, 1, 0, 0),
  #   c(1, 0, 1, 0)
  # )
  edges <- cbind(
    c(3, 1),
    c(4, 2)
  )
  expect_true(rewiring_maintains_connectedness(g, edges))
})

test_that("negative case", {
  x <- rbind(
    c(0, 1, 0, 1),
    c(0, 0, 1, 0),
    c(1, 0, 0, 0),
    c(0, 1, 1, 0)
  )
  # r <- rbind(
  #   c(0, 1, 0, 1),
  #   c(0, 0, 1, 0),
  #   c(0, 1, 0, 0),
  #   c(1, 0, 1, 0)
  # )
  g <- igraph::graph_from_adjacency_matrix(x)
  edges <- cbind(
    c(3, 1),
    c(4, 2)
  )
  expect_false(rewiring_maintains_connectedness(g, edges))
})
