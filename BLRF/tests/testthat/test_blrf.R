context ("Implement BLRF")

test_that("Implement BLRF", {
  load("train_sample.Rda")
  result1 <- blrf(Type~., train_sample, gamma=0.5, b = NULL, s=10, r=10, n_var=5, core = 1)
  result2 <- blrf(Type~., train_sample, gamma=0.5, b = NULL, s=10, r=10, n_var=5, core = 4)
  expect_is(result1, "blrf")
  expect_is(result2, 'blrf')
})
