

#' Calculate overall average misclassfied number of Trees
#'
#' @param Trees list of tree object.
#'
#' @return numeric. Average misclassified number of Trees.
#' @export
#'
#' @examples
all_eval <- function(Trees){
  list_score <- purrr::map(Trees, ~eval(.))
  purrr::reduce(list_score, `+`) / length(list_score)
}
