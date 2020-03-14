predict_ci_regression <- function(blrf, new_data, lower = 0.025, upper = 0.975){

  result <- map(blrf,
                ~ predict(., new_data)
  )

  lower_bound <-  apply(simplify2array(result), 1, quantile, prob = lower)
  upper_bound <-  apply(simplify2array(result), 1, quantile, prob = upper)

  result_ci <- map2(lower_bound, upper_bound,
                    ~{
                      paste("[", .x, ",", .y, "]")
                    })

  return (result_ci)

}
