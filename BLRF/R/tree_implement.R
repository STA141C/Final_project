

#' Build Little Random Forest (LRS)
#'
#' @param formula A formula expression.
#' @param subsample data.frame.
#' @param r numeric. Number of trees.
#' @param n numeric. Number of observations in the original data.
#' @param n_var numeric. Number of variables to subset to build one tree.
#'
#' @return list of tree object.
#' @export
#'
#' @examples
tree_implement <- function(formula, subsample, r, n, n_var){
  Trees <- purrr::map(1:r, ~{
    weight <- weights(subsample, n)
    one_tree(formula, subsample, weight, n_var)
    })
  return(Trees)
}
