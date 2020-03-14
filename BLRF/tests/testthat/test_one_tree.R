context ("creat tree object for sample data")

test_that("create one tree for each subsample", {
  load("train_sample.Rda")
  w<- weights(train_sample, 200)
  expect_equal(class(one_tree(Type~., train_sample, weights = w, n_var = 5, split = 'gini')),
               "tree")
  expect_equal(class(one_tree(Type~., train_sample, weights = w, n_var = 3, split = 'deviance')),
               "tree")
})
