test_that("Pattern is correct", {
  x3 <- rbind(
    c(0, 1, 1),
    c(1, 0, 1),
    c(1, 1, 0)
  )
  x4 <- rbind(
    c(0, 1, 2, 1),
    c(1, 0, 1, 2),
    c(2, 1, 0, 1),
    c(1, 2, 1, 0)
  )
  x6 <- rbind(
    c(0, 1, 2, 3, 2, 1),
    c(1, 0, 1, 2, 3, 2),
    c(2, 1, 0, 1, 2, 3),
    c(3, 2, 1, 0, 1, 2),
    c(2, 3, 2, 1, 0, 1),
    c(1, 2, 3, 2, 1, 0)
  )
  expect_equal(distance_to_diagonal(3), x3)
  expect_equal(distance_to_diagonal(4), x4)
  expect_equal(distance_to_diagonal(6), x6)
})
