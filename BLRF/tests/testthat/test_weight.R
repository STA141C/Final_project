context("Calculate weights of each observation in subsample")

test_that("calculate weights",{
  load("../../tinydata/train_glass_sample.Rda")
  load("../../tinydata/train_mortality_sample.Rda")
  w1 <- weights(train_glass_sample, 200)
  w2 <- weights(train_mortality_sample, 100)
  expect_equal(length(w1), nrow(train_glass_sample))
  expect_equal(sum(w1[,1]), 200)

  expect_equal(length(w2), nrow(train_mortality_sample))
  expect_equal(sum(w2[,1]), 100)
})
