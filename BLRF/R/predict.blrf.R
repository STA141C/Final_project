

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
#'
#' @return list or matrix of prediction values (or with confidence interval).
#' @export
#'
#' @examples
predict.blrf <- function(blrf, newdata, confidence = F, probability = F, lower = 0.025, upper = 0.975){
  Trees <- blrf$Trees
  Pres <- purrr::map(Trees, ~one_tree_predict(., newdata))
  final_pres <- purrr::reduce(Pres, `+`)/length(Trees)

  #cat y and label
  if(blrf$attrs$type == "factor" && !probability){
    names <- colnames(Pres[[1]])
    final_label <- as.numeric(apply(final_pres, 1, which.max))
    final_label <- names[final_label]
  }

  all_result <- NULL
  if(confidence){
    lower_bound <- apply(simplify2array(Pres), 1:2, quantile, prob = lower)
    upper_bound <- apply(simplify2array(Pres), 1:2, quantile, prob = upper)

    result_ci <- purrr::map2(lower_bound, upper_bound,
                             ~{paste("[", .x, ",", .y, "]")}
                             )
    result_ci <- as.data.frame(matrix(result_ci,
                                      nrow = nrow(newdata),
                                      dimnames = list(row.names(lower_bound),
                                                      colnames(lower_bound))))


    all_result <- cbind(ci = result_ci)
  }

  if(probability){
    all_result <- cbind(prob = final_pres, all_result)
    all_result <- select(all_result, c(1, 4, 2, 5, 3, 6))
  } else{
    all_result <- cbind(fit = final_label, all_result)
  }

  return(all_result)
}
