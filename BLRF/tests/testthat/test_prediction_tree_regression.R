context("Predictions on BLTF regression problems")

test_that('predictions on BLTF regression problems',{
  load("../../tinydata/train_mortality_sample.Rda")
  load("../../tinydata/test_mortality_sample.Rda")
  trees <- tree_implement(MORTALITY~., train_mortality_sample, r = 10, n=70, n_var= 5, split = "gini")
  result <- prediction_tree_regression(trees, test_mortality_sample)
  expect_equal(class(result), 'numeric')
})
