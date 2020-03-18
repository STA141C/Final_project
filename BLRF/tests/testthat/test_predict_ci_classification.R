context ("Get Percentile Confidence Intervals for Probabilities of Classification Prediction")

test_that("Get Percentile Confidence Intervals for Probabilities of Classification Prediction", {

  load("../../tinydata/train_glass_sample.Rda")
  load("../../tinydata/test_glass_sample.Rda")
  blrf <- blrf(Type~Al + Mg, train_glass_sample, gamma=0.5, b = NULL, s=10, r=10, n_var=2, core = 1)
  result <- predict_ci_classification(blrf, test_glass_sample, lower = 0.025, upper = 0.975)
  expect_equal(typeof(result), 'list')
  match <- c(nrow(test_glass_sample), length(unique(test_glass_sample$Type)))
  expect_equal(dim(result), match)
})

