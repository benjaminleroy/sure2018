fill_in_nas_v2 <- function(df, col_vec){
  #filling in the NAs with zeros in the sets of variables
  #where at least one variable in each row of each group has a 1
  #Parameters: df should be the dataframe. col_vec should be a vector indentify
  #the columns the function will work with
  #
  #Returns the dataframe with NAs (satisfying the above condition) replaced with
  #0s
  
  workingdf <- df[,col_vec]
  conditional <- is.na(df[,col_vec]) & 
    (apply(df[, col_vec], MARGIN = 1, sum, na.rm = TRUE) > 0)
  workingdf[conditional] <- 0
  
  df[, col_vec] <- workingdf
  
  df
}


