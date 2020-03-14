

#' Drive file to run BLRF
#'
#' @param formula A formula expression.
#' @param data data.frame. Original data.
#' @param gamma numeric. User_defined sizing factor.
#' @param b numeric. Size of subsamples.
#' @param s numeric. Number of subsamples.
#' @param r numeric. Number of trees.
#' @param n_var numeric. Number of variables to subset to build one tree.
#' @param split character string. Can be "deviance" or "gini". Default to be "gini".
#' @param core numeric. Must be positive. Number of core to use for parallel computing.
#' Default to 1, meaning no use of parallel computing.
#' If higher than 1, then implement the function with parallel computing with given number of cores.
#'
#' @return
#' @export
#'
#' @examples
implement_BLRF <- function(formula, data, gamma, b = NULL, s, r, n_var, split = "gini", core = 1){
  n <- nrow(data)
  x_var <- strsplit(as.character(formula[3]), split = "[ ]\\+[ ]")
  if(x_var != '.') {
    data <- data[, c(as.character(formula[2]), x_var[[1]])]
  }

  Subs <- subsampling(data, gamma, b, s)
  if(core == 1){
    Trees <- purrr::map(Subs, ~tree_implement(formula, subsample = ., r, n, n_var, split))
    Trees <- flatten(Trees)
  } else if(core > 0){
    plan(multiprocess, workers = core)
    Trees <- furrr::future_map(Subs, ~tree_implement(formula, subsample = ., r, n, n_var),
                               .options = future_options(scheduling = FALSE))
  }

  y <- as.character(formula[2])
  if(class(data[,y]) == "factor"){
    label <- prediction_tree_categorical(Trees, data, type = "label")
    prob <- prediction_tree_categorical(Trees, data, type = "probability")
    accuracy_m <- accuracy_mean_ci(Trees, data, lower = 0.025, upper = 0.975)

    Tree_object <- list(Trees = Trees,
                        fitted_prob = prob,
                        fitted_label = label,
                        accuracy_ci = accuracy_m)
  } else if(class(iris$Species) == "numeric"){
    fitted <- prediction_tree_regression(Trees, data)

    residuals <- fitted - data[, as.character(formula[2])]

    Tree_object <- list(Trees = Trees,
                        fitted = fitted,
                        residuals = residuals)
  }

  class(Tree_object) <- "BLRF"
  return(Tree_object)
}
