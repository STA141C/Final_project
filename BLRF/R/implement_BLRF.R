

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
  if(x_var != '.') {
    data <- data[, c(as.character(formula[2]), x_var[[1]])]
  }
  Tree_object <- list()
  class(Tree_object) <- "BLRF"

  Subs <- subsampling(data, gamma, b, s)
  if(core == 1){
    Trees <- purrr::map(Subs, ~tree_implement(formula, subsample = ., r, n, n_var))
    Trees <- flatten(Trees)
  }
  else if(core > 0){
    plan(multiprocess, workers = core)
    Trees <- furrr::future_map(Subs, ~tree_implement(formula, subsample = ., r, n, n_var),
                               .options = future_options(scheduling = FALSE))
  }

  y <- as.character(formula[2])
  if(class(iris$Species) == "factor"){
    label <- prediction_tree(Trees, data, type = "label")
    prob <- prediction_tree(Trees, data, type = "probability")
    accuracy_m <- accuracy_mean_ci(Trees, data, lower = 0.025, upper = 0.975)

    Tree_object <- list(Trees = Trees,
                        fitted_prob = prob,
                        fitted_label = label,
                        accuracy_ci = accuracy_m)
  }
  else if(class(iris$Species) == "numeric"){

  }

  return(Tree_object)

}
