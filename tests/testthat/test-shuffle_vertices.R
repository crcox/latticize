test_that("shuffling works for matrices", {
  x <- matrix(1:25, nrow = 5, ncol = 5)
  y <- shuffle_vertices(x)
  z <- unshuffle_vertices(y)
  expect_equal(z, x)
})

test_that("rows and columns are shuffled the same", {
  x <- diag(5)
  y <- shuffle_vertices(x)
  expect_equal(y, x, ignore_attr = TRUE)
})

test_that("shuffling works for igraphs", {
  x <- igraph::sample_gnm(5, 10)
  y <- shuffle_vertices(x)
  z <- unshuffle_vertices(y)
  expect_true(igraph::identical_graphs(z, x))
})
