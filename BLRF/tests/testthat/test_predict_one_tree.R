context ("prediction result for one tree")

test_that("prediction result for one tree", {
  load("train_sample.Rda")
  load('test_sample.Rda')
  w<- weights(train_sample, 200)
  control <- tree::tree.control(nobs = nrow(train_sample), minsize = 10)
  onetree <- one_tree(Type~., train_sample, weights = w, n_var = 5, split = 'gini', control = control)
  match <- c(nrow(test_sample),length(unique(test_sample$Type)))
  expect_equal(dim(one_tree_predict(onetree, test_sample)), match)
})
