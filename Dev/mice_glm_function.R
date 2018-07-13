

# beta_values<-matrix()
# 
# recruiter<- trimmed_dataset[67:71]
# 
# glm_shell <- function(x){
#  for (i in 1:5){
#   mice$i<- glm_df
# #look at coumentation to see how accessing the dataframe works   
#   logit_fun <- glm(y ~ + ageBroad + recruiter+ age::gender  data=glm_df)
#   
#   
#  } 
# }


#fakedataset
isLabour = c(0,NA,0,1,1)
ageBroad = c("18--20","21--23","18--20","18--20","21-23")
gender = c("Female","Male","Female","Female","Female")


recruiterRelationOther = c(0,0,1,NA,1)

recruiterRelationUknown = c(1,1,0,0,0)

df <- data.frame(isLabour,ageBroad,gender,recruiterRelationOther,recruiterRelationUknown)

#' Extracting final Multiple imputation
#'
#' @param mno the compress object returned from mice_new
#' @param iter integer, the number of interations given to mice_new
#' @param m integer, the number of imputations
#'
#' @return a list of 2 lists. One containing the m imputated data frames and one
#' containing the m data_corrected data frames. 
#' @export
#'
#' @examples
mice_sep <- function(mno, iter, m) {
  
  data_list <- list()
  data_corrected_list <- list()
  for (i in 1:5){
    data_list[[i]] <- mno[['data_list']][[iter]][[i]][['data']]
    data_corrected_list[[i]] <-mno[['data_list']][[iter]][[i]][['data_corrected']]
  }
  
  list("data" = data_list, "data_corrected" = data_corrected_list)
}










