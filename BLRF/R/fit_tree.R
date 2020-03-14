

fit_tree <- function(Trees, type = "label"){
  Pres <- purrr::map(Trees, ~one_tree_predict(., data))

  final_prob <- purrr::reduce(Pres, `+`)/length(Trees)
  if(type == "probability"){
    return(final_prob)
  }
  else if (type == "label"){
    final_label <- apply(final_prob, 1, which.max)
    return(final_label)
  }
}
