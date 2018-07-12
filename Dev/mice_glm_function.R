

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


imp_list=list()

mice_sep = function(mice_object){
  for(i in 1:5){
    name<-paste("iteration:", i, sep='')
  imp_data <- mice_object[["data_list"]][[i]]
  
  imp_list[[name]]<imp_data
  
  } 
}

formula() 

glm_imp = function()









