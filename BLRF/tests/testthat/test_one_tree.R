context ("creat tree object for sample data")

test_that("create one tree for each subsample", {

  ## classfication
  load("../../tinydata/train_glass_sample.Rda")
  w<- weights(train_glass_sample, 200)
  control <- tree::tree.control(nobs = nrow(train_glass_sample), minsize = 10)
  expect_equal(class(one_tree(Type~., train_glass_sample, weights = w, n_var = 5, split = 'gini', control = control)),
               "tree")
  expect_equal(class(one_tree(Type~., train_glass_sample, weights = w, n_var = 3, split = 'deviance', control = control)),
               "tree")

  ## regression
  load("../../tinydata/train_mortality_sample.Rda")
  w<- weights(train_mortality_sample, 70)
  control <- tree::tree.control(nobs = nrow(train_mortality_sample), minsize = 10)
  expect_equal(class(one_tree(MORTALITY~., train_mortality_sample, weights = w, n_var = 5, split = 'gini', control = control)),
               "tree")
  expect_equal(class(one_tree(MORTALITY~., train_mortality_sample, weights = w, n_var = 3, split = 'deviance', control = control)),
               "tree")
})
