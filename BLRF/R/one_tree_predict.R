

#' Make prediction with one tree
#'
#' @param one_tree tree object.
#' @param data data.frame object. data to be predicted.
#'
#' @return data.frame object.
#' @export
#'
#' @examples
one_tree_predict <- function(one_tree, data){
  return(predict(one_tree, data))
}
