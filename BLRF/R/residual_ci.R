

#' Calculate residual confidence interval for regression random forest.
#' Default to output 95% confidence interval.
#'
#' @param blrf blrf object.
#' @param y numeric. Response variable.
#' @param lower numeric. Define lower bound of residual confidence interval.
#' Default to be 0.025.
#' @param upper numeric. Define upper bound of residual confidence interval.
#' Default to be 0.975.
#'
#' @return matrix of residual confidence intervals for given observations.
#' @export
#'
#' @examples
residual_ci <- function(blrf, y, lower = 0.025, upper = 0.975){
  Trees <- blrf$Trees

  Pres <- purrr::map(Trees, ~predict(., newdata))
  sq_pre <- purrr::map(Pres, ~{ (. - y)^2})
  lower_bound <- apply(simplify2array(sq_pre), 1, quantile, lower)
  upper_bound <- apply(simplify2array(sq_pre), 1, quantile, upper)

  res_ci <- cbind(lwr = lower_bound, upr = upper_bound)
  return(res_ci)
}
