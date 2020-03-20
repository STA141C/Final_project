context("Plot multiple trees with given the list/vector indexes of target trees in the blrf object.")

test_that("Plot of trees",{

  load("../../tinydata/train_glass_sample.Rda")
  # classification without parallel computation
  blrf <- blrf(Type~., train_glass_sample, gamma=0.5, b = NULL, s=10, r=100, n_var=5,  core = 1)
  result <- print_trees(blrf,c(1,2,3))
  expect_equal(class(result), 'list')
})
