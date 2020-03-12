predict_BLRF <- function(blrf, new_data){

  result <- list()
  for(i in 1:length(blrf)){

    result[[i]] <- map(blrf[[i]],
        ~{
          predict(., new_data)
         }
        ) %>%
      reduce(`+`)/length(blrf[[i]])

    }

  return (reduce(result, `+`)/length(result))

}
