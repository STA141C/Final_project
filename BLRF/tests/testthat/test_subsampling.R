context("Sampling original data into s subsamples without replacement")

test_that("split data into subsamples",{

  load("train_sample.Rda")
  subsamples <- subsampling(train_sample, gamma = 0.5, b = 10, s = 10 )
  expect_equal(typeof(subsamples),'list')
  expect_equal(length(subsamples), 10)
  expect_equal(length(subsamples[[1]]), 10)
})
