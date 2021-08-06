test_that("dissimilarity", {
  expect_equal(as.vector(dissimilarity(
    data.frame(x = rep(1, 3), y = rep(1, 3), z = rep(1, 3)))), rep(0, 3))
})
