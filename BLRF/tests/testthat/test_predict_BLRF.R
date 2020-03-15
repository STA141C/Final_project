context("Predictions on BLTF classification problems")

test_that('predictions on BLTF  classification problems',{
          load("train_sample.Rda")
          load('test_sample.Rda')
          trees <- tree_implement(Type~., train_sample, r = 10, n=200, n_var= 5, split = "gini")
          result1 <- prediction_tree_categorical(trees, test_sample)
          result2 <- prediction_tree_categorical(trees, test_sample, type = "probability")
          expect_equal(class(result1), 'factor')
          expect_equal(class(result2), "matrix")
})
