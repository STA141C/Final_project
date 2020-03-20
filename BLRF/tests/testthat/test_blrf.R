context ("Implement BLRF")

test_that("Implement BLRF", {

  load("../../tinydata/train_glass_sample.Rda")

  # classification without parallel computation
  result1 <- blrf(Type~., train_glass_sample, gamma=0.5, b = NULL, s=10, r=100, n_var=5,  core = 1)
  # classification within parallel computation
  result2 <- blrf(Type~., train_glass_sample, gamma=0.5, b = NULL, s=10, r=100, n_var=5,  core = 4)
  expect_is(result1, "blrf")
  expect_is(result2, 'blrf')
  #
  #
  load("../../tinydata/train_mortality_sample.Rda")
  # regression with parallel computation
  result3 <- blrf(MORTALITY~., train_mortality_sample, gamma=0.5, b = NULL, s=10, r=100, n_var=5,  core = 1)
  # regression without parallel computation
  result4 <- blrf(MORTALITY~., train_mortality_sample, gamma=0.5, b = NULL, s=10, r=100, n_var=5,  core = 4)
  expect_is(result3, "blrf")
  expect_is(result4, 'blrf')

})
