test_that("vertices are properly unsorted", {
  x <- matrix(0, nrow = 26, ncol = 26, dimnames = list(LETTERS, LETTERS))
  expect_equal(rownames(latticize(x, 1e-5)), LETTERS)
})

test_that("handle matrix", {
  x <- rbind(
    c(0, 0, 0, 0),
    c(0, 0, 0, 0),
    c(1, 0, 0, 0),
    c(0, 1, 0, 0)
  )
  d <- rbind(
    c(0, 1, 2, 1),
    c(1, 0, 1, 2),
    c(2, 1, 0, 1),
    c(1, 2, 1, 0)
  )
  r <- rbind(
    c(0, 0, 0, 0),
    c(0, 0, 0, 0),
    c(0, 1, 0, 0),
    c(1, 0, 0, 0)
  )
  expect_equal(attempt_latticization(x, d, TRUE)$x, r)
})
