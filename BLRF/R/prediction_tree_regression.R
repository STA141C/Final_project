

#' Make prediction with list of trees for regression
#'
#' @param Trees list of tree objects.
#' @param data data.frame. data to be predicted.
#'
#' @return vector. Aggregated prediction.
#' @export
#'
#' @examples
prediction_tree_regression <- function(Trees, newdata){
  Pres <- purrr::map(Trees, ~one_tree_predict(., newdata))

  final_pre <- purrr::reduce(Pres, `+`)/length(Trees)
  return(final_pre)
}
