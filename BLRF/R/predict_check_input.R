


#' Checking if all inputs are valid for function predict_blrf.
#'
#' @param blrf blrf object.
#' @param confidence logical. If TURE, then output confidence interval.
#' @param lower numeric. If confidence is TRUE, then define lower bound of ci.
#' @param upper numeric. If confidence is TRUE, then define upper bound of ci.
#'
#' @return logic.
#' @export
#'
#' @examples
predict_check_input <- function(blrf, confidence, probability , lower, upper){

  if(confidence & (blrf$attrs$type == "factor") & (probability  == F)){
    stop("No confidence interval available for label response")
  }

  if(!is.numeric(c(lower, upper))){
    stop("'lower' or 'upper' is not numeric value, input numeric values for both of them.")
  }

  if(!all(dplyr::between(c(lower, upper), 0, 1))){
    stop("'lower' or 'upper' is out of bound. Enter number between 0 and 1.")
  }
  return(TRUE)
}
