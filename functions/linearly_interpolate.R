#linearly_interpolate(df, var_cols) carrys back the first observed value to 
#fill in missing values at the beginning, carrys forward the last observed to 
#fill in missing values at the end, and draws a line to estimate missing values
#in between observations.
#interpolate_line and fill_na are helper functions that draws a line between 
#observations and fills in the NAs in a row, respectively.
#
#parameters: df should be a dataframe or matrix. var_cols should be a list of
#integer vectors. Each vector should have the column indicies, in order, of the 
#group of variables one wants to impute. These columns must contain only numeric
#values. 
#
#returns: a numeric matrix

require(dplyr)

interpolate_line <- function(entry){
  #draws a straight line between to data points to fill in missing values
  if (length(entry) == 1) {
    return(entry)
  }else{
    left_point <- entry %>% (function(x){(length(x):1)*!is.na(x)}) %>% 
      which.max
    right_point <- entry[-left_point] %>% 
      (function(x){(length(x):1)*!is.na(x)}) %>% 
      which.max
    difference <- entry[right_point + 1] - entry[left_point]
    
    entry[left_point:right_point + 1] <- entry[left_point] + 
      difference*(1:right_point)/right_point
    
    entry[(right_point+1):length(entry)] <- 
      interpolate_line(entry[(right_point+1):length(entry)])
    
    return(entry)}
}

fill_na <- function (entry, var_cols) {
  #Loops over the list of vectors of column indicies to be imputed
  #carries first known observation back and last known observation forward to
  #fill in NAs and calls interpolate_line for missing values in between
  for(variables in var_cols){
    dat <- entry[variables]
    
    begining <- dat %>% (function(x){(length(x):1)*!is.na(x)}) %>% 
      which.max
    
    ending <- dat %>% (function(x){(1:length(x))*!is.na(x)}) %>% 
      which.max
    
    left_tail <- variables[1:length(dat) < begining]
    entry[left_tail] <- dat[begining]
    right_tail <- variables[1:length(dat) > ending]
    entry[right_tail] <- dat[ending]
    
    left <- variables[begining]
    right <- variables[ending]
    
    entry[left:right] <- interpolate_line(dat[begining:ending])
  }
  entry
}

linearly_interpolate<- function(df, var_cols) {
  #apply loop over the rows of the data frame calling fill_na 
  df %>% 
    apply(MARGIN = 1, fill_na, var_cols) %>% 
    t
  }