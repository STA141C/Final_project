

#' Calculate weights of each observation in subsample
#' based on multinomial method
#'
#' @param subsample data.frame
#' @param n size of original dataset
#'
#' @return matrix
#'
#' @examples
#' data(iris)
#' weights(iris, nrow(iris)*10)

weights <- function(subsample, n){
  weights <- stats::rmultinom(1, n, rep(1, nrow(subsample)))
  return(weights)
}
