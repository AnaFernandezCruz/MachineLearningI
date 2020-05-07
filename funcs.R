library(ggplot2)

normalize_scaling <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

normalize_standar <- function(x, mean, sd) {
  return ((x - mean)/sd) 
}

accuracy <- function(x){
  sum(diag(x)/(sum(rowSums(x)))) * 100
}


calc_error_rate <- function(predicted.value, true.value){
  return(mean(true.value!=predicted.value))
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