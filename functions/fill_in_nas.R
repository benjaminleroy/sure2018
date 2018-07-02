fill_in_nas <- function(df, var_list){
  #filling in the NAs with zeros in the sets of variables
  #where at least one variable in each row of each group has a 1 
  df_filled_in <- df
  for(j in var_list) {
    for (i in 1:nrow(df_filled_in[,j])){
      if (sum(df_filled_in[i,j], na.rm = TRUE)>0){
        df_filled_in[i,j][as.vector(is.na(df_filled_in[i,j]))]<-0
      }
    }
  }
  df_filled_in
}