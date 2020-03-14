predict_ci <- function(blrf, new_data, lower = 0.025, upper = 0.975){

  result <- map(blrf,
      ~ predict(., new_data)
      )

  return (apply(simplify2array(result), 1:2, quantile, prob = c(lower, upper)))

  #return (result)
  #return (purrr::reduce(result, `+`)/length(result))

}
