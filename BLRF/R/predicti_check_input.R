


#' Checking if all inputs are valid for function predict_blrf.
#'
#' @param blrf blrf object.
#' @param newdata an optional data frame, list or environment.
#' @param confidence logical. If TURE, then output confidence interval.
#' @param probability logical. If TURE, then output probability matrix.
#' @param lower numeric. If confidence is TRUE, then define lower bound of ci.
#' @param upper numeric. If confidence is TRUE, then define upper bound of ci.
#'
#' @return logic.
#' @export
#'
#' @examples
predicti_check_input <- function(blrf, newdata, confidence, probability, lower, upper){

  if(!is.numeric(c(lower, upper))){
    stop("'lower' or 'upper' is not numeric value, input numbers for both of them.")
  }

  if(blrf$attrs$type == "numeric" & probability == TRUE){
    warning("No probability avaiable for regression rf, output fitted values")
  }
  if(!all(between(c(lower, upper), 0, 1))){
    stop("'lower' or 'upper' is out of bound. Enter number between 0 and 1.")
  }
  return(TRUE)
}
