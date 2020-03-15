context("Confidence interval for mean classification accuracy rate")

test_that("CI for mean classification accuracy rate",{

  load("train_sample.Rda")
  load('test_sample.Rda')
  trees <- tree_implement(Type~., train_sample, r = 10, n=200, n_var= 5, split = "gini")
  result <- accuracy_mean_ci(trees, test_sample, lower = 0.025, upper = 0.975)
  expect_equal(class(result), "matrix")
  expect_equal(typeof(result), 'double')
})
