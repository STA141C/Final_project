context ("Get Percentile Confidence Intervals for values of Regression Prediction")

test_that("Get Percentile Confidence Intervals for values of Regression Prediction", {

  load("../../tinydata/train_mortality_sample.Rda")
  load("../../tinydata/test_mortality_sample.Rda")
  blrf <- blrf(MORTALITY~., train_mortality_sample, gamma=0.5, b = NULL, s=10, r=10, n_var=2, core = 1)
  result <- predict_ci_regression(blrf, test_mortality_sample, lower = 0.025, upper = 0.975)
  expect_equal(typeof(result), 'list')
})
