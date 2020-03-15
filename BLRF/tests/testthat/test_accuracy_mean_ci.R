context("Confidence interval for mean classification accuracy rate")

test_that("CI for mean classification accuracy rate",{

  load("../../tinydata/train_glass_sample.Rda")
  load("../../tinydata/test_glass_sample.Rda")
  trees <- tree_implement(Type~., train_glass_sample, r = 10, n=200, n_var= 5, split = "gini")
  result <- accuracy_mean_ci(trees, test_glass_sample, lower = 0.025, upper = 0.975)
  expect_equal(class(result), "matrix")
  expect_equal(typeof(result), 'double')
})
