

#' Make prediction with list of trees with given type of calculating the prediction
#'
#' @param Trees list of tree objects.
#' @param data data.frame. data to be predicted.
#' @param type character string. Define the method to aggreate prediction. default to be "average". Choice from "average" to "frequency".
#'
#' @return data.frame. Aggregated prediction.
#' @export
#'
#' @examples
trees_predict <- function(Trees, data, type = "average"){
  Pres <- purrr::map(Trees, ~one_tree_predict(., data))

  if(type == "average"){
    final_predict <- purrr::reduce(Pres, `+`)/length(Trees)
  }
  else if (type == "frequency"){
    #?
    #final_predict <- purrr::reduce(Pres, rbind)/length(Trees)
  }
  return(final_predict)
}
