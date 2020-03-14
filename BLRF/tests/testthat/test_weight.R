context("Calculate weights of each observation in subsample")

test_that("calculate weights",{

  load("train_sample.Rda")
  w <- weights(train_sample, 200)
  expect_equal(length(w), nrow(train_sample))
  expect_equal(sum(w[,1]), 200)
})
