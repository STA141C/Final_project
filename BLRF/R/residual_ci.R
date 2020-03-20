

#' Calculate average accuracy and its CI
#'
#' @description Function is used to compute average residual given input of "numeric" type of \code{blrf}
#' and its the confidence interval of residuals aggreating all Trees. Default to output 95 percentage
#' confidence interval.
#'
#' @param blrf Object of class inheriting from \code{blrf}.
#' @param data data.frame.
#' @param lower numeric. Define lower bound of residual confidence interval.
#' Default to be 0.025.
#' @param upper numeric. Define upper bound of residual confidence interval.
#' Default to be 0.975.
#'
#' @return matrix of residual confidence intervals for given observations.
#' @export
#'
#' @examples
#' data(mtcars)
#' tt <- blrf(mpg~., mtcars, gamma = 0.7, s = 10, r = 100, n_var = 2)
#' predict.blrf(tt, mtcars)
residual_ci <- function(blrf, data, lower = 0.025, upper = 0.975){
  Trees <- blrf$Trees
  y_name <- as.character(blrf$Call[2])
  Pres <- purrr::map(Trees, ~predict(., data))
  sq_pre <- purrr::map(Pres, ~{ (. - data[, y_name])^2})
  lower_bound <- apply(simplify2array(sq_pre), 1, quantile, lower)
  upper_bound <- apply(simplify2array(sq_pre), 1, quantile, upper)

  res_ci <- cbind(lwr = lower_bound, upr = upper_bound)
  return(res_ci)
}
