

#' Drive file to run BLRF
#'
#' @param formula A formula expression.
#' @param data data.frame. Original data.
#' @param gamma numeric. User_defined sizing factor.
#' @param b numeric. Size of subsamples.
#' @param s numeric. Number of subsamples.
#' @param r numeric. Number of trees.
#' @param n_var numeric. Number of variables to subset to build one tree.
#'
#' @return
#' @export
#'
#' @examples
implement_BLRF <- function(formula, data, gamma, b = NULL, s, r, n_var){
  n <- nrow(data)
  Subs <- subsampling(data, gamma, b, s)
  Trees <- purrr::map(Subs, ~tree_implement(formula, subsample = ., r, n, n_var))
  misclass <- all_eval(Trees)
  return()
}
