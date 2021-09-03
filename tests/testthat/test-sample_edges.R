test_that("returns correct format", {
  x <- diag(3)
  expect_true(is.matrix(sample_edges(x, 2)))
  expect_equal(nrow(sample_edges(x, 3)), 2)
  expect_equal(ncol(sample_edges(x, 3)), 3)
})

test_that("samples withot replacement", {
  x <- diag(3)
  expect_error(sample_edges(x, 4))
})

test_that("handles igraph", {
  x <- diag(3)
  g <- igraph::graph_from_adjacency_matrix(x)
  expect_true(is.matrix(sample_edges(g, 2)))
  expect_equal(nrow(sample_edges(g, 3)), 2)
  expect_equal(ncol(sample_edges(g, 3)), 3)
})

test_that("all vertices are unique", {
  # note, the edge at [2, 3] should never be selected, because it shares a
  # vertex with each of the other two. It cannot form a valid pair with any
  # other edge.
  x <- rbind(
    c(0, 0, 0, 0),
    c(1, 0, 1, 0),
    c(0, 0, 0, 0),
    c(0, 0, 1, 0)
  )
  y <- sample_edges_with_unique_vertices(x, 2)
  y <- y[, order(y[1, ])]
  expect_false(any(duplicated(y)))
  expect_equal(y, matrix(c(2, 1, 4, 3), nrow = 2,
                         dimnames = list(c("source", "target"), NULL)))
})
