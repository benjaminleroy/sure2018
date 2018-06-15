na_mat <- function(df){
  #Returns a indicator matrix for the missing values in a data frame as well as
  #matrices for the covariance and correlation of missingness between variables
  #Arguements:df should be a data frame 
  
  na_matrix <- sapply(df, is.na)
  na_cov <- cov(na_matrix)
  constant_variables <- integer()
  for (i in 1:nrow(na_cov)){
    if (na_cov[i,i] == 0) {
      constant_variables <- c(constant_variables, i)
    }
  }

  na_cor <- cov2cor(na_cov[-constant_variables,-constant_variables])
  
  return(list(na_matrix, na_cov, na_cor))
}