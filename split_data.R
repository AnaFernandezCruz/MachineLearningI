split_data <- function(df, factor){
  ran <- sample(1:nrow(df), factor * nrow(df))
  train <- df[ran,]
  test <- df[-ran,]
  result <- list("blockH"= train, "blockL" = test)
  return(result)
}