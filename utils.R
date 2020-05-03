get_cols_nan <- function(df){
  list_na <- colnames(df)[ apply(df, 2, anyNA) ]
  return(list_na)
}

fill_nan_mean <- function(df){
  for(i in 1:ncol(df)){
    df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
  }
  return(df)
}

as_factor <- function(df, cols){
  for(i in cols){
    df[[i]] <- as.factor(df[[i]])
  }
  return(df)
}

draw_confusion_matrix <- function(tab, tab_Actual, tab_Predict){
  confusion_matrix <- as.data.frame(tab)
  
  Actual <- factor(confusion_matrix[[tab_Actual]])
  Predichos <- factor(confusion_matrix[[tab_Predict]])
  Y      <- confusion_matrix$Freq
  df <- data.frame(Actual, Predichos, Y)
  
  ggplot(data =  df, mapping = aes(x = Actual, y = Predichos)) +
    geom_tile(aes(fill = Y), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
    scale_fill_gradient(low = "blue", high = "red") +
    theme_bw() + theme(legend.position = "none")
}

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

calc_error_rate <- function(predicted.value, true.value){
  return(mean(true.value!=predicted.value))
}

draw_scatter <- function(df, var1, var2){
  myColors <- brewer.pal(5,"Set1")
  names(myColors) <- levels(df$GrupoPrecio)
  colScale <- scale_colour_manual(name = "GrupoPrecio",values = myColors)
  ggplot(data = df, aes(x,y,colour = GrupoPrecio)) +
  geom_point(mapping = aes(x = df[[var1]], y = df[[var2]]))
}

