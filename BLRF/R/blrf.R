
#' blrf is used to fit Random Forest with Bag of Little Random Forests.
#' It can be used to carry out both regression and classification trees.
#'
#'
#' @param formula an object of class "formula".
#' @param data an optional data frame, list or environment
#' (or object coercible by as.data.frame to a data frame) containing the variables in the model.
#' @param gamma numeric. User_defined sizing factor.
#' @param b numeric. An optional number of size of subsamples.
#' @param s numeric. number of subsamples.
#' @param r numeric. Number of trees.
#' @param n_var numeric. Number of variables to subset to build one tree.
#' @param split character string. Can be "deviance" or "gini". Default to be "gini".
#' @param core numeric. Must be positive. Number of core to use for parallel computing.
#' Default to 1, meaning no use of parallel computing.
#' If higher than 1, then implement the function with parallel computing with given number of cores.
#'
#' @return blrf object with different attributes given type of response variable.
#' @export
#'
#' @examples
blrf <- function(formula, data, gamma, b = NULL, s, r, n_var, split = "gini", core = 1){
  n <- nrow(data)

  x_var <- strsplit(as.character(formula[3]), split = "[ ]\\+[ ]")[[1]]
  y <- as.character(formula[2])
  implement_check_input(x_var, y, formula, data, gamma, b, s, r, n_var, split, core)

  if("." %in% x_var) {
    x_var <- unique(c(colnames(data), x_var[x_var != "."]))
  }
  data <- data[, c(y, x_var)]

  Subs <- subsampling(data, gamma, b, s)
  if(core == 1){
    Trees <- purrr::map(Subs, ~tree_implement(formula, subsample = ., r, n, n_var, split))
    Trees <- flatten(Trees)
  } else if(core > 0){
    plan(multiprocess, workers = core)
    Trees <- furrr::future_map(Subs, ~tree_implement(formula, subsample = ., r, n, n_var),
                               .options = future_options(scheduling = FALSE))
  }

  Tree_object <- list(Call = formula,
                      attrs = list(gamma = gamma, b = b, s = s, r = r,
                                    n_var = n_var, split = split))
  if(class(data[,y]) == "factor"){
    label <- prediction_tree_categorical(Trees, data, type = "label")

    prob <- prediction_tree_categorical(Trees, data, type = "probability")

    accuracy_m <- accuracy_mean_ci(Trees, data, lower = 0.025, upper = 0.975)

    Tree_object$Trees <- Trees
    Tree_object$fitted_prob <- prob
    Tree_object$fitted_label <- label
    Tree_object$accuracy_ci <- accuracy_m
  } else if(class(data[,y]) == "numeric"){
    fitted <- prediction_tree_regression(Trees, data)

    residuals <- fitted - data[, as.character(formula[2])]

    Tree_object$Trees <- Trees
    Tree_object$fitted <- fitted
    Tree_object$residuals <- residuals
  }

  class(Tree_object) <- "blrf"
  return(Tree_object)
}
