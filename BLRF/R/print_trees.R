

#' Plot of trees
#'
#' Plot multiple trees with given the list/vector indexes of target trees in the blrf object.
#'
#' @param blrf Object of class inheriting from \code{blrf}.
#' @param tree_list numeric. indext of trees needs to make plot, ie. c(1,2).
#' @param splits logical. If TRUE the splits are labelled.
#' @param label The name of column in the frame component of x.
#' to be used to label the nodes. Can be NULL to suppress node-labelling.
#' @param all logical. By default, splits of tree are labeled, ie. interior nodes are also labelled.
#' @param pretty the manipulation used for split labels involving attributes.
#' @param cex numeric. NULL and NA are equivalent to 1.0.
#'
#' @details Options' deatils except \code{blrf} can be found in tree::text.tree().
#'
#' @return list of plots.
#' @export
#'
#' @examples
#' data(iris)
#' tt <- blrf(Species~., iris, gamma = 0.7, s = 10, r = 100, n_var = 2)
print_trees <- function(blrf, tree_list, splits = TRUE, label = "yval", all = TRUE,
                        pretty = 1, cex = 1){
  tt <- blrf$Trees[tree_list]
  map(tt, ~plot_one_tree(., splits, label, all, pretty, cex))
}
