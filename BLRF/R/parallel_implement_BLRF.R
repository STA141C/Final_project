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

                               map(seq_len(r),
                                   ~{
                                     weight <- rmultinom(1, n, rep(1, nrow(subsample)))
                                     #one_tree(formula, subsample, weight, n_var)
                                     var <- colnames(subsample)[!colnames(subsample) %in% as.character(formula[2])]
                                     list_var <- sample(var, n_var, replace = F)
                                     f <- stats::as.formula(paste(formula[2], '~', paste(list_var, collapse = '+')))
                                     Tree <- tree::tree(f, data = subsample, weights = weight, wts = T)
                                     Tree
                                   }
                                   )
                               #tree_implement(formula, subsample, r, n, n_var)

                              },
                      .options = future_options(scheduling = FALSE)

                      )


  return (Trees)
}
