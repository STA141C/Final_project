


#' Checking if all inputs are valid for function predict_blrf.
#'
#' @param blrf blrf object.
#' @param confidence logical. If TURE, then output confidence interval.
#' @param probability logical. If TRUE, then output will be predict probability for factor
#' type of blrf. If FALSE, then the output will be predict label for "factor"
#' type of blrf or predict value for "numeric" type of blrf.
#' @param pretty logical. If pretty is TRUE, then output character string output of ci for factor response,
#' not available for numeric response.
#' @param lower numeric. If confidence is TRUE, then define lower bound of ci.
#' @param upper numeric. If confidence is TRUE, then define upper bound of ci.
#'
#' @return logic.
#'
#' @examples
predict_check_input <- function(blrf, confidence, probability, pretty, lower, upper){

  if(confidence & (blrf$attrs$type == "factor") & (probability  == F)){
    stop("No confidence interval available for label response")
  }

  if(confidence & blrf$attrs$type == "numeric" & pretty){
    warning("`pretty` is not available for numeric response")
  }

  if(!is.numeric(c(lower, upper))){
    stop("'lower' or 'upper' is not numeric value, input numeric values for both of them.")
  }

  if(!all(dplyr::between(c(lower, upper), 0, 1))){
    stop("'lower' or 'upper' is out of bound. Enter number between 0 and 1.")
  }
  return(TRUE)
}
