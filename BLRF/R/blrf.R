
#' Fitting random forest with bag of little bootstraps
#'
#' blrf is used to fit Random Forest with Bag of Little Random Forests.
#' It can be used to carry out both regression and classification trees.
#'
#'
#' @param formula an object of class \code{formula}.
#' @param data an optional data frame, list or environment
#' (or object coercible by as.data.frame to a data frame) containing the variables in the model.
#' @param gamma numeric. User_defined sizing factor.
#' @param b numeric. An optional number of size of subsamples.
#' @param s numeric. number of subsamples.
#' @param r numeric. Number of trees.
#' @param n_var numeric. Number of variables to randomly subset to build one tree.
#' @param split character string. Can be "deviance" or "gini". Default to be "gini".
#' @param control control object based on tree::tree.control() function from "tree" package.
#' Default set to call tree::tree.control(), ie, using default values from tree::tree.control()).
#' @param core numeric. Must be positive. Number of core to use for parallel computing.
#' Default to 1, meaning no use of parallel computing.
#' If higher than 1, then implement the function with parallel computing with given number of cores.
#'
#' @details \code{gamma} is used to indicate the factor of the size to subsample the original data,
#' ie. \eqn{n^{gamma}}.
#' Optional \code{b} indicates the size of each subsample and each sample are set to be equal sized if b is given.
#' \code{n_var} is the number of variables to be used in the tree and
#' is used to randomly select variables when each tree is generated.
#' If \code{b} and \code{gamma} are both given, \code{b} is used to generate subsamples.
#' \code{split} can be "gini" or "deviance" and is the splitting criterion to use to generate each tree.
#' Detail argument of \code{control} can be found in tree::tree.control().
#'
#' @return Object of class inheriting from \code{blrf}.
#'
#' @export
#'
#' @examples
#' data(iris)
#' blrf(Species~., iris, gamma = 0.7, s = 10, r = 100, n_var = 2)
blrf <- function(formula, data, gamma, b = NULL, s, r, n_var, split = "gini",
                 control = tree::tree.control(nobs = nrow(data), minsize = 10),
                 core = 1){
  n <- nrow(data)
  x_var <- strsplit(as.character(formula[3]), split = "[ ]\\+[ ]")[[1]]
  y <- as.character(formula[2])
  implement_check_input(x_var, y, formula, data, gamma, b, s, r, n_var, split, core)

  if("." %in% x_var) {
    # x_var <- unique(c(colnames(data), x_var[x_var != "."]))
    data <- data
  }else{
    data <- data[, c(y, x_var)]
  }

  Subs <- subsampling(data, gamma, b, s)
  if(core == 1){
    Trees <- purrr::map(Subs, ~tree_implement(formula, subsample = ., r, n, n_var, split, control = control))
    Trees <- purrr::flatten(Trees)
  } else if(core > 1){

    # tryCatch(expr = {future::plan(future::multiprocess, workers = core)},
    #          error = function(e) {
    #                               future::plan(future::multicore, workers = core)})

    if("Darwin" %in% Sys.info()['sysname']){
      future::plan(future::multicore, workers = core)
    }else{ future::plan(future::multiprocess, workers = core)}

      Trees <- furrr::future_map(Subs, ~{
      #tree_implement(formula, subsample = ., r, n, n_var,split),

      subsample <- .
      purrr::map(1:r, ~{

        weight <- weights(subsample, n)
        #control <- tree::tree.control(nobs = nrow(train_sample), minsize = 10)
        #one_tree(formula, subsample, weight, n_var, split, control)
        var <- colnames(subsample)[!colnames(subsample) %in% as.character(formula[2])]
        list_var <- sample(var, n_var, replace = F)
        f <- stats::as.formula(paste(formula[2], '~', paste(list_var, collapse = '+')))
        tree::tree(f, data = subsample, weights = weight, wts = T, split = split, control = control)
        }
        )

      }, .options = furrr::future_options(scheduling = FALSE))

      Trees <- purrr::flatten(Trees)

  }

  Tree_object <- list(Call = formula,
                      attrs = list(gamma = gamma, b = b, s = s, r = r,
                                   n_var = n_var, split = split,
                                   control = control, type = class(data[,y])))
  if(class(data[,y]) == "factor"){
    Tree_object$Trees <- Trees

    label <- predict.blrf(Tree_object, data)

    prob <- predict.blrf(Tree_object, data, probability = T)

    accuracy_m <- accuracy_mean_ci(Tree_object, data, lower = 0.025, upper = 0.975)

    Tree_object$fitted_prob <- prob
    Tree_object$fitted_label <- label
    Tree_object$accuracy_ci <- accuracy_m
  } else if(class(data[,y]) == "numeric"){
    Tree_object$Trees <- Trees

    fitted <- predict.blrf(Tree_object, data)

    residuals <- fitted - data[, as.character(formula[2])]

    residuals_interval <- residual_ci(Tree_object, data)

    Tree_object$fitted <- fitted
    Tree_object$residuals <- residuals
    Tree_object$residuals_ci <- residuals_interval
  }

  class(Tree_object) <- "blrf"
  return(Tree_object)
}
