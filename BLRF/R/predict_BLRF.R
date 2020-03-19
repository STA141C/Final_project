predict_BLRF <- function(blrf, new_data){

  result <- list()
  for(i in 1:length(blrf$Trees)){

    result[[i]] <- map(blrf$Trees[[i]],
        ~{
          predict(., new_data)
         }
        ) %>%
      reduce(`+`)/length(blrf$Trees[[i]])

    }

  return (reduce(result, `+`)/length(result))

}
