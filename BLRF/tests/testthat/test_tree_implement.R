context ("Build Little Random Forest (LRS) for One Subsample")

test_that("Build Little Random Forest (LRS) for One Subsample",{
  load("../../tinydata/train_glass_sample.Rda")
  trees <- tree_implement(Type~RI + Na, train_glass_sample, r = 10, n=200, n_var= 5, split = "gini")
  expect_is(trees, "list")
})
