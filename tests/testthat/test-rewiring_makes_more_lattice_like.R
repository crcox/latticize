test_that("multiplication works", {
  d <- rbind(
    c(0, 1, 2, 1),
    c(1, 0, 1, 2),
    c(2, 1, 0, 1),
    c(1, 2, 1, 0)
  )
  x <- rbind(
    c(0, 0, 0, 0),
    c(0, 0, 0, 0),
    c(1, 0, 0, 0),
    c(0, 1, 0, 0)
  )
  y <- rbind(
    c(0, 0, 0, 0),
    c(0, 0, 0, 0),
    c(0, 1, 0, 0),
    c(1, 0, 0, 0)
  )
  edges_x <- cbind(
    c(3, 1),
    c(4, 2)
  )
  edges_y <- cbind(
    c(3, 2),
    c(4, 1)
  )
  expect_true(rewiring_makes_more_lattice_like(x, d, edges_x))
  expect_false(rewiring_makes_more_lattice_like(x, d, edges_y))
})
