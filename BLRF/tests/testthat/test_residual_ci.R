context("Calculate residual confidence interval for regression random forest")

test_that("CI for prediction residuals",{

  load("../../tinydata/train_mortality_sample.Rda")
  load("../../tinydata/test_mortality_sample.Rda")
  rg_blrf <- blrf(MORTALITY~., train_mortality_sample, gamma=0.5, b = NULL, s=10, r=100, n_var=5,  core = 1)
  result <- residual_ci(rg_blrf, train_mortality_sample, lower = 0.025, upper = 0.975)
  expect_equal(class(result),'matrix')
})
