context('Confusion matarix values for each response variable for one tree.')

test_that("create confusion matrix for prediction result", {
  load("train_sample.Rda")
  load('test_sample.Rda')
  w<- weights(train_sample, 200)
  control <- tree::tree.control(nobs = nrow(train_sample), minsize = 10)
  onetree <- one_tree(Type~., train_sample, weights = w, n_var = 5, split = 'gini',control = control)
  result <- Confusion_one_tree(onetree, test_sample)
  expect_equal(class(result), 'matrix')
  expect_type(result,'integer')
})
