context ("Make prediction with given data. User can indicate
         if output needs to be probability,or include confidence interval")

test_that("prediction with/without confidence interval",{


  ### classification
  load("../../tinydata/train_glass_sample.Rda")
  load("../../tinydata/test_glass_sample.Rda")
  test_glass_sample_x <- subset(test_glass_sample, select = -c(Type))
  test_glass_sample_y <- test_glass_sample[c('Type')]

  cls_blrf <- blrf(Type~., train_glass_sample, gamma=0.5, b = NULL, s=10, r=100, n_var=5,  core = 4)
  result1 <- predict(cls_blrf, test_glass_sample_x, confidence = T, probability = T)
  expect_equal(class(result1), "data.frame")

  result2 <- predict(cls_blrf, test_glass_sample_x, confidence = F, probability = F)
  expect_equal(class(result2), "matrix")

  load("../../tinydata/train_mortality_sample.Rda")
  load("../../tinydata/test_mortality_sample.Rda")
  test_mortality_sample_x <- subset(test_mortality_sample, select = -c(MORTALITY))
  test_mortality_sample_y <- test_mortality_sample[c('MORTALITY')]

  rg_blrf <- blrf(MORTALITY~., train_mortality_sample, gamma=0.5, b = NULL, s=10, r=100, n_var=5,  core = 4)
  result3 <- predict(rg_blrf, test_mortality_sample_x, confidence = T, probability = F, pretty = F,
                     lower = 0.025, upper = 0.975)
  expect_equal(class(result3), "data.frame")
})
