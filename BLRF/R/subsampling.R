

#' Sampling original data into s subsamples without replacement
#'
#' @param data data.frame. Original data.
#' @param gamma numeric. User_defined sizing factor.
#' @param b numeric. Size of subsamples.
#' @param s numeric. Number of subsamples.
#'
#' @return list of data.frame
#' @export
#'
#' @examples
subsampling <- function(data, gamma, b = NULL, s){
  if(is.null(b)) b <- nrow(data)^gamma
  n <- nrow(data)
  subindexes <- replicate(s, sample(n, b, replace = F), simplify = F)
  subsamples <- purrr::map(subindexes, ~data[.,])
  return(subsamples)
}
