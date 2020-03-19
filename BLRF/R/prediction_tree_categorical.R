

#' Make prediction with list of trees with given type of calculating the prediction
#'
#' @param Trees list of tree objects.
#' @param data data.frame. data to be predicted.
#' @param type character string. Define the output to be probability matrix or vector of label, default to be "label". Choice from "label" to "probability".
#'
#' @return data.frame. Aggregated prediction.
#' @export
#'
#' @examples
prediction_tree_categorical <- function(Trees, newdata, type = "label"){
  Pres <- purrr::map(Trees, ~one_tree_predict(., newdata))

  final_prob <- purrr::reduce(Pres, `+`)/length(Trees)
  if(type == "probability"){
    return(final_prob)
  } else if (type == "label"){
    final_label <- apply(final_prob, 1, which.max)
    return(as.factor(final_label))
  }
}
