test_that("handles matrix", {
  x <- diag(2)
  expect_equal(edge2vertices(x, 4),
               matrix(c(2, 2), nrow = 2,
                      dimnames = list(c("source", "target"), NULL)))
})

test_that("handles igraph", {
  x <- igraph::graph_from_adjacency_matrix(diag(2))
  expect_equal(edge2vertices(x, 2),
               matrix(c(2, 2), nrow = 2,
                      dimnames = list(c("source", "target"), NULL)))
})
