getColumnsWithNan <- function(df){
  list_na <- colnames(df)[ apply(df, 2, anyNA) ]
  return(list_na)
}

fillNanWithMean <- function(df){
  for(i in 1:ncol(df)){
    df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
  }
  return(df)
}