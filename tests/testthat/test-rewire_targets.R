test_that("handle matrix", {
  x <- rbind(
    c(0, 0, 0, 0),
    c(1, 0, 0, 0),
    c(0, 0, 0, 0),
    c(0, 0, 1, 0)
  )
  edges <- cbind(
    c(2, 1),
    c(4, 3)
  )
  r <- rbind(
    c(0, 0, 0, 0),
    c(0, 0, 1, 0),
    c(0, 0, 0, 0),
    c(1, 0, 0, 0)
  )
  expect_equal(rewire_targets(x, edges), r)
})

test_that("handle igraph", {
  x <- rbind(
    c(0, 0, 0, 0),
    c(1, 0, 0, 0),
    c(0, 0, 0, 0),
    c(0, 0, 1, 0)
  )
  g <- igraph::graph_from_adjacency_matrix(x)
  edges <- cbind(
    c(2, 1),
    c(4, 3)
  )
  r <- rbind(
    c(0, 0, 0, 0),
    c(0, 0, 1, 0),
    c(0, 0, 0, 0),
    c(1, 0, 0, 0)
  )
  gr <- igraph::graph_from_adjacency_matrix(r)
  expect_true(igraph::identical_graphs(rewire_targets(g, edges), gr))
})
