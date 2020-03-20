

#' Plot of one tree
#'
#' Plot one tree with given text object of plot.
#'
#'
#' @param Tree tree object.
#' @param splits logical. If TRUE the splits are labelled.
#' @param label The name of column in the frame component of x.
#' to be used to label the nodes. Can be NULL to suppress node-labelling.
#' @param all logical. By default, splits of tree are labeled, ie. interior nodes are also labelled.
#' @param pretty the manipulation used for split labels involving attributes.
#' @param cex numeric. NULL and NA are equivalent to 1.0.
#'
#' @details Options' deatils except `Tree` can be found in tree::text.tree.
#'
#' @return plot object.
#'
#' @examples
plot_one_tree <- function(Tree, splits = TRUE, label = "yval", all = TRUE,
                          pretty = 1, cex = 1){
  plot(Tree, type = "uniform")
  text(Tree, splits = splits, label = label, all = all, pretty = pretty, cex = cex)
  plot <- grDevices::recordPlot()
  return(plot)
}
