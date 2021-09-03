test_that("targets are swapped", {
  x <- rbind(source = c(2, 8), target = c(4, 5))
  r <- rbind(source = c(2, 8), target = c(5, 4))
  expect_equal(swap_targets(x), r)
})
