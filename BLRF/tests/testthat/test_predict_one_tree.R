context ("prediction result for one tree")

test_that("prediction result for one tree", {

  ## classification
  load("../../tinydata/train_glass_sample.Rda")
  load("../../tinydata/test_glass_sample.Rda")
  w<- weights(train_glass_sample, 200)
  control <- tree::tree.control(nobs = nrow(train_glass_sample), minsize = 10)
  onetree <- one_tree(Type~., train_glass_sample, weights = w, n_var = 5, split = 'gini', control = control)
  match <- c(nrow(test_glass_sample),length(unique(test_glass_sample$Type)))
  expect_equal(dim(one_tree_predict(onetree, test_glass_sample)), match)

  ## regression
  load("../../tinydata/train_mortality_sample.Rda")
  load("../../tinydata/test_mortality_sample.Rda")
  w<- weights(train_mortality_sample, 70)
  control <- tree::tree.control(nobs = nrow(train_mortality_sample), minsize = 10)
  onetree <- one_tree(MORTALITY~., train_mortality_sample, weights = w, n_var = 5, split = 'gini', control = control)
  expect_equal(typeof(one_tree_predict(onetree, test_mortality_sample)), "double")
})
