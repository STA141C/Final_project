context("Confidence interval for mean classification accuracy rate")

test_that("CI for mean classification accuracy rate",{

  load("../../tinydata/train_glass_sample.Rda")
  load("../../tinydata/test_glass_sample.Rda")
  result1 <- blrf(Type~., train_glass_sample, gamma=0.5, b = NULL, s=10, r=100, n_var=5,  core = 1)
  result <- accuracy_mean_ci(result1, test_glass_sample, lower = 0.025, upper = 0.975)
  expect_equal(class(result), "matrix")
  expect_equal(typeof(result), 'double')
})
