

#' Drive file to run BLRF
#'
#' @param formula A formula expression.
#' @param data data.frame. Original data.
#' @param gamma numeric. User_defined sizing factor.
#' @param b numeric. Size of subsamples.
#' @param s numeric. Number of subsamples.
#' @param r numeric. Number of trees.
#' @param n_var numeric. Number of variables to subset to build one tree.
#' @param core numeric. Number of core.
#'
#' @return
#' @export
#'
#' @examples
implement_BLRF <- function(formula, data, gamma, b = NULL, s, r, n_var, core = 1){
  n <- nrow(data)
  x_var <- strsplit(as.character(formula[3]), split = "[ ]\\+[ ]")
  data <- data[, x_var[[1]]]
  Subs <- subsampling(data, gamma, b, s)
  if(core = 1){
    Trees <- purrr::map(Subs, ~tree_implement(formula, subsample = ., r, n, n_var))
  }
  else if(core > 0){

  }
  #misclass <- all_eval(Trees)
  return(Trees)
}
