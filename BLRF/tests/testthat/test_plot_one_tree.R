context("Plot one tree with given text object of plot")

test_that("plot one tree",{

  load("../../tinydata/train_glass_sample.Rda")
  # classification without parallel computation
  blrf <- blrf(Type~., train_glass_sample, gamma=0.5, b = NULL, s=10, r=100, n_var=5,  core = 1)
  result <- plot_one_tree(blrf$Trees[[1]])
  expect_equal(class(result), "recordedplot")
})
