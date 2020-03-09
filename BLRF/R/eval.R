

#' Calculate misclassified number of one tree
#'
#' @param one_tree tree object.
#'
#' @return numeric.
#' @export
#'
#' @examples
eval <- function(one_tree){
  tree::misclass.tree(one_tree)
}
