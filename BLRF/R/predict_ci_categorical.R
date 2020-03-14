predict_ci_categorical <- function(blrf, new_data, lower = 0.025, upper = 0.975){

  result <- map(blrf,
                ~ predict(., new_data)
  )

  lower_bound <-  apply(simplify2array(result), 1:2, quantile, prob = lower)
  upper_bound <-  apply(simplify2array(result), 1:2, quantile, prob = upper)



  result_ci <- map2(lower_bound, upper_bound,
                    ~{
                      paste("[", .x, ",", .y, "]")
                    }) %>%
    matrix(nrow = nrow(new_data), dimnames = list(row.names(lower_bound), colnames(lower_bound))) %>%
    as.data.frame()



  return (result_ci)

}
