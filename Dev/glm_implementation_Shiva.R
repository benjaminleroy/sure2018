sure_path <- getwd()
load(paste(sure_path, "/data/mice_new_obj_marginal.Rdata", sep = ""))
library(tidyverse)
library(rpart)
source(paste(sure_path, "/functions/mice_glm_function.R", sep = ""))
source(paste(sure_path, "/functions/beta_functions.R", sep = ""))


### Building the regression formula
load(paste(sure_path, "/data/mice_new_obj.Rdata", sep = ""))
column_names <- colnames(mice_new_obj[['data_list']][[10]][[1]][['data']])
rm("mice_new_obj")
citz_features <- grep("Citz_+", column_names, value = TRUE) %>% paste(collapse = " + ")
exp_features <- grep("Exp_+", column_names, value = TRUE)[] %>% paste(collapse = " + ")
meansOfControl <- grep("meansOfControl+", column_names, value = TRUE)[-18] %>% paste(collapse = " + ")
migration <- grep("Migration_+", column_names, value = TRUE) %>% paste(collapse = " + ")
populationgroup <- grep("Population_+", column_names, value = TRUE) %>% paste(collapse = " + ")
RecruiterRelationship <- c(grep("recruiterRelation+", column_names, value = TRUE)[-5], 
                           "isAbduction") %>% paste(collapse = " + ")

imp_formula <- paste("isForcedLabour ~ gender + ageBroad", citz_features, exp_features,
                     meansOfControl, migration, populationgroup, RecruiterRelationship,
                    sep = " + ") %>% as.formula()

imp_formula_interaction <- paste("isForcedLabour ~ gender*ageBroad", citz_features, exp_features,
                     meansOfControl, migration, populationgroup, RecruiterRelationship,
                     sep = " + ") %>% as.formula()

working_mno <- mice_new_obj_marginal

working_mno <- bind_country_cols(working_mno, 10, 5, sure_path = sure_path)


#Fitting glm model
data_list <- mice_sep(working_mno, 10, 5, stratify = TRUE, 
                      strat_vars = c("gender", "ageBroad", "Country_of_Exploit_2L",
                                     "Country_of_Citizenship_2L"))

glm_list <- mno_regression(data_list[["data_corrected"]][["training"]], 
                           imp_formula_interaction, glm, family = "binomial")

##Evaluating the model:
#Getting a matrix of the beta values from the GLM list
betas <- beta_matrix(glm_list)

#finding the variance in betas between each imputation
betas_variance <- apply(betas, MARGIN = 2, var)

#Visualizing the beta matrix
vis_beta_mat(betas)

#building roc objects:
roc_list <- list()
for(i in 1:length(glm_list)){
  roc_list[[i]] <- pROC::roc(data_list[["data_corrected"]][["test"]][[i]][["isForcedLabour"]],
                        predict(glm_list[[i]], type = "response", 
                                newdata = data_list[["data_corrected"]][["test"]][[i]]))
}

for(i in roc_list){pROC::auc(i) %>% print}

for(i in 1:length(glm_list)){
  hoslem.test(data_list[["data_corrected"]][["test"]][[i]][["isForcedLabour"]],
              predict(glm_list[[i]], type = "response", 
                      newdata = data_list[["data_corrected"]][["test"]][[i]])) %>%
    print
}

###Building a composite variable LabourOrSex from isForcedLabour and isSexualExploit
for(set in c("training", "test")){
  for(i in 1:5){
    holder_df <- data_list[["data_corrected"]][[set]][[i]]
    
    holder_df$LabourOrSex <- NA
    
    holder_df[holder_df$isForcedLabour == 1 & !is.na(holder_df$isForcedLabour),"LabourOrSex"] <- TRUE
    holder_df[holder_df$isSexualExploit == 1 & !is.na(holder_df$isSexualExploit),"LabourOrSex"] <- FALSE
    holder_df[,"LabourOrSex"] <- as.factor(unlist(holder_df[,"LabourOrSex"])*1)
    data_list[["data_corrected"]][[set]][[i]] <- holder_df
  }
}

#Refitting the glm model with a new formula:
imp_formula_interaction2 <- paste("LabourOrSex ~ gender*ageBroad", citz_features, exp_features,
                                 meansOfControl, migration, populationgroup, RecruiterRelationship,
                                 sep = " + ") %>% as.formula()

glm_list2 <- mno_regression(data_list[["data_corrected"]][["training"]], 
                            imp_formula_interaction2, glm, family = binomial)

#Getting a matrix of the beta values from the GLM list
betas2 <- beta_matrix(glm_list2)

#finding the variance in betas between each imputation
betas_variance2 <- apply(betas2, MARGIN = 2, var)

#building roc objects:
roc_list2 <- list()
for(i in 1:length(glm_list)){
  roc_list2[[i]] <- pROC::roc(data_list[["data_corrected"]][["test"]][[i]][["LabourOrSex"]],
                             predict(glm_list2[[i]], type = "response", 
                                     newdata = data_list[["data_corrected"]][["test"]][[i]]))
}
for(i in roc_list2){pROC::auc(i) %>% print}

for(i in 1:length(glm_list2)){
  hoslem.test(data_list[["data_corrected"]][["test"]][[i]][["LabourOrSex"]],
              predict(glm_list2[[i]], type = "response", 
                      newdata = data_list[["data_corrected"]][["test"]][[i]])) %>%
    print
}


### using stepwise regression

#this took more than 90 mins:
load(paste(sure_path, "/data/shrunken_glms.Rdata", sep = ""))

roc_list_shrunk <- list()
for(i in 1:length(shrunk_glm_list)){
  roc_list_shrunk[[i]] <- pROC::roc(data_list[["data_corrected"]][["test"]][[i]][["LabourOrSex"]],
                              predict(shrunk_glm_list[[i]], type = "response", 
                                      newdata = data_list[["data_corrected"]][["test"]][[i]]))
}
for(i in roc_list_shrunk){pROC::auc(i) %>% print}

for(i in 1:length(shrunk_glm_list)){
  hoslem.test(data_list[["data_corrected"]][["test"]][[i]][["isForcedLabour"]],
              predict(shrunk_glm_list[[i]], type = "response", 
                      newdata = data_list[["data_corrected"]][["test"]][[i]])) %>%
    print
}


#####Classification Trees:

tree_list <- mno_regression(data_list[["data_corrected"]][["training"]], 
               imp_formula, rpart)

#Creating a confusion Matrix:
caret::confusionMatrix(predict(tree_list[[1]], 
                               newdata = data_list[["data_corrected"]][["test"]][[1]], 
                               type = "class"), 
                       data_list[["data_corrected"]][["test"]][[1]][["isForcedLabour"]])


#Using the composite variable:
imp_formula2 <- paste("LabourOrSex ~ gender + ageBroad", citz_features, exp_features,
                     meansOfControl, migration, populationgroup, RecruiterRelationship,
                     sep = " + ") %>% as.formula()

tree_list2 <- mno_regression(data_list[["data_corrected"]][["training"]], 
               imp_formula2, rpart)

#confusion matrix:
caret::confusionMatrix(predict(tree_list2[[1]], 
                               newdata = data_list[["data_corrected"]][["test"]][[1]], 
                               type = "class"), 
                       data_list[["data_corrected"]][["test"]][[1]][["LabourOrSex"]])
