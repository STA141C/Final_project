

#' Implement random forest once
#'
#' @param formula an object of class "formula".
#' @param subsample data.frame.
#' @param weights list.
#' @param n_var numeric. Number of variables to sample.
#' @param split character string. Can be "deviance" or "gini".
#'
#' @return tree object.
#'
#' @examples
one_tree <- function(formula, subsample, weights, n_var, split){
  var <- colnames(subsample)[!colnames(subsample) %in% as.character(formula[2])]
  list_var <- sample(var, n_var, replace = F)
  f <- stats::as.formula(paste(formula[2], '~', paste(list_var, collapse = '+')))
  Tree <- tree::tree(f, data = subsample, weights = weights, wts = T, split = split)
  return(Tree)
}
