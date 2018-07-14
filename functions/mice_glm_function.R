#' Extracting final Multiple imputation
#'
#' @param mno the compress object returned from mice_new
#' @param iter integer, the number of interations given to mice_new
#' @param m integer, the number of imputations
#' @param stratify logical, if true each list will have both a training and test set
#' @param strat_vars a vector of variables to stratify on 
#' @param seed integer, a seed to be feed to set.seed
#' @param size a number between 0 and 1, controls the size of the test set
#'
#' @return a list of 2 lists. One containing the m imputated data frames and one
#' containing the m data_corrected data frames. 
#' @export
#'
#' @examples
mice_sep <- function(mno, iter, m, stratify = FALSE, strat_vars = NULL, 
                     seed = 1, size = 0.6) {
  
  data_list <- list()
  data_corrected_list <- list()
  if (stratify){
    set.seed(seed = seed)
    for (i in 1:m){
      strata <- stratified(mno[['data_list']][[iter]][[i]][['data']], 
                           strat_vars, size = size, bothSets = TRUE)
      data_list[["training"]][[i]] <- strata$SAMP1
      data_list[["test"]][[i]] <- strata$SAMP2
      
      strata_corrected <- stratified(mno[['data_list']][[iter]][[i]][['data_corrected']], 
                           strat_vars, size = size, bothSets = TRUE)
      data_corrected_list[["training"]][[i]] <- strata_corrected$SAMP1
      data_corrected_list[["test"]][[i]] <- strata_corrected$SAMP2
    }
  }else{
    for (i in 1:m){
      data_list[[i]] <- mno[['data_list']][[iter]][[i]][['data']]
      data_corrected_list[[i]] <-mno[['data_list']][[iter]][[i]][['data_corrected']]
      }
    
    }
  return(list("data" = data_list, "data_corrected" = data_corrected_list))
}



#' Mice new object regression function
#' 
#' A function to call the same regression function on each imputed dataset.
#'
#' @param mno the compress object returned from mice_new 
#' @param opt 1 for the uncorrected data and 2 for the corrected data
#' @param formula a forumla object which will be feed to the choosen method
#' @param method a function either lm, glm, gam or rpart
#' @stratified a logical if TRUE the function will attempt to fit the supplied 
#' regression model on the training set only
#' @param ... additional arguments to feed into method 
#'
#' @return list of object corresponding to 
#' @export
#'
#' @examples
mno_regression <- function(mno, opt, formula, method, stratified = FALSE,...){
  if(stratified){
    data_list <- mice_sep(mice_sep)[opt]
    df_list <- data_list[["training"]]
  }else {df_list <- mice_sep(mice_sep)[opt]}
  result_list <- list()
  
  i <- 0
  for (imp_df in df_list){
    i <- i + 1
    result_list[[i]] <- method(formula = formula, data = imp_df, ...)
  }
  
  result_list
}


#' Method to work with the regression objects list
#'
#' A function to call any function on the list of regression objects returned by
#' the above function.
#'
#' @param obj_list a list of objects returned by regression functions
#' @param method a function to be used on each object, e.g. coef or summary
#' @param ... additional arguements for method
#'
#' @return a list of results from the supplied method
#' @export
#'
#' @examples
glm_list_wrapper <- function(obj_list, method, ...){
  i <- 0
  for(obj in obj_list){
    i <- i + 1
    result_list[[i]] <- method(obj_list, ...)
  }
  
  result_list
}





