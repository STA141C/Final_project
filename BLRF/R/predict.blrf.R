

#' Make prediction with given data. User can indicate if output needs to be probability
#' or include confidence interval.
#'
#' @param blrf blrf object.
#' @param newdata data.frame. Data to be
#' @param confidence logical. If TRUE, then output will include confidence interval.
#' @param probability logical. If TRUE, then output will be predict probability for factor
#' type of blrf. If FALSE, then the output will be predict label for "factor"
#' type of blrf or predict value for "numeric" type of blrf.
#' @param lower numeric. If confidence is TRUE, then define lower bound of ci.
#' @param upper numeric. If confidence is TRUE, then define upper bound of ci.
#' @param pretty logical. If pretty is TRUE, then output character string output of ci.
#'
#' @return list or matrix of prediction values (or with confidence interval).
#' @export
#'
#' @examples
predict.blrf <- function(blrf, newdata, confidence = F, probability = F, pretty = F,
                         lower = 0.025, upper = 0.975){
  predict_check_input(blrf, confidence, lower, upper)

  Trees <- blrf$Trees
  Pres <- purrr::map(Trees, ~predict(., newdata))
  final_pres <- purrr::reduce(Pres, `+`)/length(Trees)

  #cat y and label
  if(blrf$attrs$type == "factor" && !probability){
    final_label <- as.numeric(apply(final_pres, 1, which.max))
    final_label <- names[final_label]
  }

  all_result <- NULL
  result_ci <- NULL
  if(confidence){
    lower_bound <- apply(simplify2array(Pres), 1:2, quantile, prob = lower)
    upper_bound <- apply(simplify2array(Pres), 1:2, quantile, prob = upper)

    if(pretty){
      result_ci <- purrr::map2(lower_bound, upper_bound,
                               ~{paste("[", .x, ",", .y, "]")})
      result_ci <- as.data.frame(matrix(result_ci,
                                      nrow = nrow(newdata),
                                      dimnames = list(row.names(lower_bound),
                                                      colnames(lower_bound))))

    } else{
      result_ci <- cbind(lwr = lower_bound, upr = upper_bound)
      colnames(result_ci) <- c(paste0("lwr.", colnames(lower_bound)),
                               paste0("upr.", colnames(upper_bound)))
      #index <- unlist(map(1:n_v, ~c(., .+n_v)))
      #result_ci <- result_ci[, index]
    }
    all_result <- cbind(ci = result_ci)
  }

  if(probability){
    if(blrf$attrs$type == "numeric"){
      warning("No probability avaiable for regression rf, output fitted values")
      all_result <- cbind(fit = final_pres, all_result)
    }
    else{
      all_result <- cbind(prob = final_pres, all_result)
    }
  } else{
    if(blrf$attrs$type == "numeric"){
      all_result <- cbind(fit = final_pres, all_result)
    } else {
      all_result <- cbind(fit = final_label, all_result)
    }
  }

  names <- colnames(Pres[[1]])
  n_v <- length(names)


  if(confidence & probability & !pretty){
    all_result <- all_result[, unlist(purrr::map(1:n_v, ~c(., .+n_v, .+2*n_v)))]
  } else if(confidence & probability & pretty){
    print("h")
    all_result <- all_result[, unlist(purrr::map(1:n_v, ~c(., .+n_v)))]
  }

  return(all_result)
}
