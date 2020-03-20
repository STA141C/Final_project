context('Confusion matarix values for each response variable for one tree.')

test_that("create confusion matrix for prediction result", {
  load("../../tinydata/train_glass_sample.Rda")
  load("../../tinydata/test_glass_sample.Rda")
  w<- weights(train_glass_sample, 200)
  control <- tree::tree.control(nobs = nrow(train_glass_sample), minsize = 10)
  onetree <- one_tree(Type~., train_glass_sample, weights = w, n_var = 5, split = 'gini',control = control)
  result <- Confusion_one_tree(onetree, test_glass_sample)
  expect_equal(class(result), 'list')
  expect_type(result,'list')
})
