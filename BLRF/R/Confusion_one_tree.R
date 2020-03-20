

#' Calculate confusion matrix for each response variable for one tree.
#'
#'
#' @param one_tree tree object.
#' @param data data.frame object.
#'
#' @return list of matrix and overall number of correct labeled observations.
#' Confusion matarix values for each response variable for one tree.
#'
#' @examples
Confusion_one_tree <- function(one_tree, data){
  #f1, tpr,
  #tree::misclass.tree(one_tree)

  prob <- predict(one_tree, data)
  cats <- colnames(prob)
  pre <- colnames(prob)[apply(prob, 1, which.max)]
  y <- as.character(as.formula(one_tree)[2])
  confusion_matrix <- c()
  for(i in seq(length(cats))){
    tp <- sum(pre == cats[i] &
                data[, y] == cats[i])
    confusion_matrix <- rbind(confusion_matrix, cbind(tp = tp,
                                    fp = sum(pre == cats[i]) - tp,
                                    fn = sum(data[, y] == cats[i]) - tp,
                                    tn = sum(pre != cats[i] &
                                               data[, y] != cats[i])))
  }
  rownames(confusion_matrix) <- cats

  overall = sum(data[, y] == pre)
  return(list(confusion_matrix, overall))
}
