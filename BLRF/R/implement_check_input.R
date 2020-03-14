
#' Checking if all inputs are valid for function implement_BLRF.
#' If not valid input exists, terminate program and output error message.
#'
#' @param x character vector. vector of x variables' names.
#' @param y  character. y variable name.
#' @param formula A formula expression.
#' @param data an optional data frame, list or environment
#' (or object coercible by as.data.frame to a data frame) containing the variables in the model.
#' @param gamma numeric. User_defined sizing factor.
#' @param b numeric. Size of subsamples.
#' @param s numeric. Number of subsamples.
#' @param r numeric. Number of trees.
#' @param n_var numeric. Number of variables to subset to build one tree.
#' @param split character string. Need to be "deviance" or "gini".
#' @param core numeric. Must be positive. Number of core to use for parallel computing.
#'
#' @return logic. TRUE if no error, otherwise terminate the program.
#'
#' @examples
implement_check_input <- function(x, y, formula, data, gamma, b, s, r, n_var, split, core){

  #validate formula input
  if(class(formula) != "formula") stop("`formula` must be 'formula' object", call. = FALSE)

  # validate number of col in data
  # when num of col in data is 1
  if(is.null(ncol(data))){
    if(any(x == ".")) { # when x var contains "."
      stop("'.' in formula and no 'data' argument", call. = FALSE)
    } else { #when x var is not "."
      stop("numeric 'envir' arg not of length one", call. = FALSE)
    }
  } else if(!any(x == ".")){ #when x is not ".", ie. more than 1 x var and data has more than 1 col
      for(i in seq(length(x))){
        if(!x[i] %in% colnames(data)){
          stop(sprintf("object '%s' not found", x[i]), call. = FALSE)
        }
      }
  }
  #if  y variable in data
  if(!y %in% colnames(data)) stop(sprintf("object '%s' not found", y), call. = FALSE)

  #validate if numeric inputs are positive
  ############???????????????????? if need to validate them to be integer?
  if(!is.null(b)){
    if(any(c(gamma, b, s, r, n_var, core) <= 0)){
      stop("All values of `gamma`, `b`, `s`, `r`, `n_var`, `core` must be positive", call. = FALSE)
    }
  } else{
    if(any(c(gamma, s, r, n_var, core) <= 0)){
      stop("All values of `gamma`, `s`, `r`, `n_var`, `core` must be positive", call. = FALSE)
    }
  }

  if(!any(c("gini", "deviance") %in% split)) stop("`split` must be 'deviance' or 'gini'", call. = FALSE)

  if(split == "deviance") warning("'deviance' split may produce corrupt trees. Using to 'gini' if happens", call. = FALSE)

  return(TRUE)
}
