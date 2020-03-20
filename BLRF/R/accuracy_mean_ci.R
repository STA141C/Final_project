

#' Calculate average accuracy and its CI
#'
#' @description Function is used to compute average accuracy given input of "factor" type of \code{blrf}
#' and its the confidence interval of accuracy aggreating all Trees.
#' Will call Confusion_one_tree function to calculate confusion matrix for one tree.
#' Default to output 95 percentage confidence interval.
#'
#' @param blrf Object of class inheriting from \code{blrf}.
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
#' tt <- blrf(Species~., iris, gamma = 0.7, s = 10, r = 100, n_var = 2)
#' accuracy_mean_ci(tt, data, lower = 0.025, upper = 0.975)
accuracy_mean_ci <- function(blrf, data, lower = 0.025, upper = 0.975){
  Trees <- blrf$Trees
  list <- purrr::map(Trees, ~Confusion_one_tree(., data))

  confusion_matrix_es <- map(list, ~.[[1]])
  accuracy_matrix <- purrr::map_dfc(confusion_matrix_es,
                                     ~{(.[, "tp"]+.[, "tn"])/nrow(data)})
  accuracy_ci <- apply(accuracy_matrix, 1, function(x) stats::quantile(x, c(lower, upper)))
  mean_accuracy <- apply(accuracy_matrix, 1, mean)

  ACCURACY <- rbind(mean = mean_accuracy, accuracy_ci)

  overall <- purrr::map_dbl(list, ~.[[2]])/nrow(data)
  m_acc <- sum(overall)/(length(overall))
  lw <- quantile(overall, lower)
  up <- quantile(overall, upper)

  ACCURACY <- cbind(ACCURACY, overall = rbind(m_acc, lw, up))

  colnames(ACCURACY) <- c(rownames(confusion_matrix_es[[1]]), "overall")

  return(ACCURACY)
}
