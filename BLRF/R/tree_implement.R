

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
tree_implement <- function(formula, subsample, r, n, n_var, split, control){
  Trees <- purrr::map(1:r, ~{
    weight <- weights(subsample, n)
    control <- tree::tree.control(nobs = nrow(train_sample), minsize = 10)
    one_tree(formula, subsample, weight, n_var, split, control)
    })
  return(Trees)
}
