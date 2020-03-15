#' Get Percentile Confidence Intervals for values of Regression Prediction
#'
#' @param blrf fitted model object of class blrf.
#' @param new_data data frame containing the values at which predictions are required.
#' @param lower numeric; lower percentile of confidence intervals.
#' @param upper nemeric; upper percentile of confidence intervals.
#'
#' @return list of confidence intervals.
#' @export
#'
#' @examples
predict_ci_regression <- function(blrf, new_data, lower = 0.025, upper = 0.975){

  result <- map(blrf$Trees,
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
