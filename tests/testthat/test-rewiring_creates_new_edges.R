test_that("multiplication works", {
  x <- rbind(
    c(0, 0, 0, 0),
    c(1, 0, 0, 0),
    c(0, 0, 0, 0),
    c(0, 0, 1, 0)
  )
  y <- rbind(
    c(0, 0, 0, 0),
    c(1, 0, 1, 0),
    c(0, 0, 0, 0),
    c(0, 0, 1, 0)
  )
  edges <- cbind(
    c(2, 1),
    c(4, 3)
  )
  expect_true(rewiring_creates_new_edges(x, edges))
  expect_false(rewiring_creates_new_edges(y, edges))
})
