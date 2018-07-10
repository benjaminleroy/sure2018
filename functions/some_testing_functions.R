#' Sense test for the number of 1's in for a given set of variables 
#'
#' @param df a data frame
#' @param var_cols a vector of column indicies or names for each variable in the 
#' set
#' @param a a number which each row will be compared to
#' @param equal a logical, if true the function will return the number of rows 
#' with 'a' 1's in the set of variables
#' @param number a logical, if true the function will return the number 1's in 
#' all rows in the set of variables
#' @param less_than a logical, if true the function will return the number of rows 
#' with less than 'a' 1's in the set of variables
#' @param greater_than a logical, if true the function will return the number of rows 
#' with more than 'a' 1's in the set of variables
#'
#' @return depends on the which parameter is set equal to true
#' @export
#'
#' @examples
row_sum_test <- function(df, var_cols, a, equal = FALSE, number = FALSE, 
                         less_than = !greater_than, greater_than = !less_than, na.rm = FALSE) {
  
  row_sums <- apply(df[,var_cols], MARGIN = 1, function(x){x %>% as.numeric %>% sum(na.rm = na.rm)})
  
  if (equal) {
    row_sums %>% (function(x) {x == a}) %>% sum %>% return
  } else if (number) {
    row_sums %>% sum %>% return
  } else if (less_than) {
    print("less_than")
    row_sums %>% (function(x) {x < a}) %>% sum %>% return
  } else if (greater_than) {
    print("greater_than")
    row_sums %>% (function(x) {x > a}) %>% sum %>% return
  }
}

