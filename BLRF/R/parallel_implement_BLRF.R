#' Drive file to run BLRF
#'
#' @param formula A formula expression.
#' @param data data.frame. Original data.
#' @param gamma numeric. User_defined sizing factor.
#' @param b numeric. Size of subsamples.
#' @param s numeric. Number of subsamples.
#' @param r numeric. Number of trees.
#' @param n_var numeric. Number of variables to subset to build one tree.
#' @param worker numeric. Number of workers.
#' @return
#' @export
#'
#' @examples
parallel_implement_BLRF <- function(formula, data, gamma, b = NULL, s, r, n_var, worker){

  n <- nrow(data)
  b <- floor(n^gamma)

  plan(multiprocess, workers = worker)

  Trees <- future_map(seq_len(s),

                             ~{
                               subindex <- sample(n, b, replace = F)
                               subsample <- data[subindex,]
                               tree_implement(formula, subsample, r, n, n_var)

                              }

                      )
#  ### test
#  plan(multiprocess, workers = worker)
#
#  Trees <- future_map(seq_len(s),
#
#                      ~{
#                        map(seq_len(2),
#                            ~tree(formula, subsample))
#
#                      }
#
#  )

  return (Trees)
}
