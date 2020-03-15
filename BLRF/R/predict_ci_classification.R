#' Get Percentile Confidence Intervals for Probabilities of Classification Prediction
#'
#' @param blrf fitted model object of class blrf.
#' @param new_data data frame containing the values at which predictions are required.
#' @param lower numeric; lower percentile of confidence intervals.
#' @param upper nemeric; upper percentile of confidence intervals.
#'
#' @return data frame of confidence intervals.
#' @export
#'
#' @examples
predict_ci_classification <- function(blrf, new_data, lower = 0.025, upper = 0.975){

  Trees <- blrf$Trees

  result <- purrr::map(Trees,
                       ~ predict(., new_data)
  )

  lower_bound <-  apply(simplify2array(result), 1:2, quantile, prob = lower)
  upper_bound <-  apply(simplify2array(result), 1:2, quantile, prob = upper)



  result_ci <- purrr::map2(lower_bound, upper_bound,
                    ~{
                      paste("[", .x, ",", .y, "]")
                    }) %>%
    matrix(nrow = nrow(new_data), dimnames = list(row.names(lower_bound), colnames(lower_bound))) %>%
    as.data.frame()



  return (result_ci)

}
