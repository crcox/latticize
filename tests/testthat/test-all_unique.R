test_that("handles vector", {
  x <- 1:8
  y <- rep(4, 2)
  expect_true(all_unique(x))
  expect_false(all_unique(y))
})

test_that("handles matrix", {
  x <- matrix(1:8, nrow = 2)
  y <- matrix(rep(1:4, 2), nrow = 2)
  expect_true(all_unique(x))
  expect_false(all_unique(y))
})

test_that("handles array", {
  x <- array(1:8, dim = c(2, 2, 2))
  y <- array(rep(1:4, 2), dim = c(2, 2, 2))
  expect_true(all_unique(x))
  expect_false(all_unique(y))
})
