predict_ci <- function(blrf, new_data, lower, upper){

  result <- list()
  for(i in 1:length(blrf)){

    result[[i]] <- map_dbl(blrf[[i]],
                       ~{
                         predict(., new_data)
                        }
                          ) %>%
      quantile(c(lower, upper))

  }

  return (reduce(result, `+`)/length(result))

}
