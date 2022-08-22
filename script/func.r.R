################################################################################
## plot box plot function for feature engineering
################################################################################
boxplot_features <- function(df, x = Label, y, title){
  ggplot(df, aes(x=x, y=y)) +   
    ## Change outlier, color, shape and size
    geom_boxplot(notch = TRUE, 
                 outlier.colour = "#ff7f50", 
                 outlier.shape = 16,
                 outlier.size = 4)+
    labs(title = paste("Box Plot of feature engineering \"", title, "\""))+
    # Rotate the box plot
    coord_flip()
}
################################################################################
## plot Distribution of feature engineering vs ham/spam
################################################################################
hist_features <- function(df, x="TextLength", fill="Label", title= "SMS length", binwidth = 5){
    ggplot(df, aes(x=x, fill=fill)) +
      geom_histogram(binwidth=binwidth) +
      scale_fill_manual(values=c("#003767","#ff7f50")) +
      labs(title = paste("Distribution of", title))
}

################################################################################
## join two dataframe
################################################################################
join_sentiment <- function(x, y, label= "affin") { 
  ## bind nrc sentiment dataframe with hotels dataframe
  y <- x %>% 
  inner_join(y, by = "id")
  
  ## print the numbers of reviews that has no sentiment evaluation
  print (paste("There are [",
               nrow(x) - nrow(y),
               "] Reviews that have no sentiment attached to it according to", 
               label, "dataset.")
         )
  
  ## return joined data frame 
  return(y)
}
################################################################################
## join two dataframe
################################################################################
join_sentiment_all <- function(x, y, label= "affin") { 
  ## bind nrc sentiment dataframe with hotels dataframe
  y <- x %>% 
    left_join(y, by = "id")
  
  ## print the numbers of reviews that has no sentiment evaluation
  print (paste("There are [",
               nrow(x) - nrow(y),
               "] Reviews that have no sentiment attached to it according to", 
               label, "dataset.")
  )
  
  ## return joined data frame 
  return(y)
}