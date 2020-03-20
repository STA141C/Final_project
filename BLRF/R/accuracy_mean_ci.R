

#' Calculate overall average accuracy and
#' the confidence interval of accuracy aggreating all Trees.
#' Will call Confusion_one_tree function to calculate confusion matrix for one tree.
#' Default to output 95% confidence interval.
#'
#' @param blrf object.
#' @param data data.frame object. Data to be predicted for accuracy.
#' @param lower numeric. Define lower bound of quantile.
#' Default to be 0.025.
#' @param upper numeric. Define upper bound of quantile.
#' Default to be 0.975.
#'
#' @return matrix. Average accuracy and confidence interval for each response variable.
#' @export
#'
#' @examples
accuracy_mean_ci <- function(blrf, data, lower = 0.025, upper = 0.975){
  Trees <- blrf$Trees
  confusion_matrix_es <- purrr::map(Trees, ~Confusion_one_tree(., data))

  accuracy_matrix <- purrr::map_dfc(confusion_matrix_es,
                                     ~{(.[, "tp"]+.[, "tn"])/nrow(data)})
  accuracy_ci <- apply(accuracy_matrix, 1, function(x) stats::quantile(x, c(lower, upper)))
  mean_accuracy <- apply(accuracy_matrix, 1, mean)

  ACCURACY <- rbind(mean = mean_accuracy, accuracy_ci)
  colnames(ACCURACY) <- rownames(confusion_matrix_es[[1]])


  return(ACCURACY)
  # tpr <- purrr::map_dfc(confusion_matrix_es, ~{.[, "tp"]/(.[, "tp"] + .[, "fn"])})
  # f1 <- purrr::map_dfc(confusion_matrix_es,
  #                     ~{.[, "tp"]+.[, "tn"]})

  #purrr::reduce(confusion_matrix_es, `+`) / length(list_score)
}
