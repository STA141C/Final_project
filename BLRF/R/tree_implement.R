

#' Build Little Random Forest (LRS) for One Subsample.
#'
#' @param formula an object of class "formula".
#' @param subsample data.frame.
#' @param r numeric. Number of trees.
#' @param n numeric. Number of observations in the original data.
#' @param n_var numeric. Number of variables to subset to build one tree.
#' @param split character string. Can be "deviance" or "gini".
#'
#' @return list of tree object.
#'
#' @examples
tree_implement <- function(formula, subsample, r, n, n_var, split,
                           control = tree::tree.control(nobs = n, mincut = 5, minsize = 10, mindev = 0.01)){
  Trees <- purrr::map(1:r, ~{
    weight <- weights(subsample, n)
    one_tree(formula, subsample, weight, n_var, split, control)
    })
  return(Trees)
}
